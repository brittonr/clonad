{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- Module      : Clonad
-- Description : A Claude Foreign Function Interface for Haskell
-- License     : BSD-3-Clause
-- Stability   : experimental
--
-- The 'Clonad' monad provides a natural-language foreign function interface
-- to the Claude API. Instead of implementing functions in Haskell, you
-- describe their behaviour in plain English and let Claude compute the
-- result at runtime.
--
-- The type signature of your function serves as the contract: inputs are
-- serialised to a textual representation, sent to Claude alongside your
-- specification, and the response is parsed back into the expected Haskell
-- type.
--
-- @
-- -- FFI to C:
-- foreign import ccall "sin" c_sin :: Double -> Double
--
-- -- FFI to Claude:
-- sentiment :: Text -> Clonad Double
-- sentiment = clonad "return a sentiment score from -1.0 to 1.0"
-- @
module Clonad
  ( -- * The Clonad Monad
    Clonad,
    MonadClonad,
    ClonadEnv (..),
    Backend (..),

    -- * Errors
    ClonadError (..),

    -- * Domain Types
    ApiKey,
    mkApiKey,
    unApiKey,
    ModelId,
    mkModelId,
    unModelId,
    Temperature,
    mkTemperature,
    unTemperature,

    -- * The Claude FFI
    clonad,
    clonad_,
    tryClonad,
    tryClonad_,

    -- * Serialisation
    ClonadParam (..),
    ClonadReturn (..),
    AsJSON (..),
    ViaShow (..),
    ViaRead (..),

    -- * Environment
    defaultEnv,
    mkEnv,
    mkOllamaEnv,
    mkOpenAIEnv,

    -- * Runner
    runClonad,
    runClonadEither,

    -- * Combinators
    withModel,
    withTemperature,
    withSystemPrompt,
  )
where

import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), ask, local)
import Data.Aeson (FromJSON (..), eitherDecodeStrict, object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Typeable (Typeable, typeRep)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import Network.HTTP.Req
import Numeric.Natural (Natural)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Domain Types (Newtypes for Type Safety)
-- ---------------------------------------------------------------------------

-- | An API key for authentication with LLM providers.
newtype ApiKey = ApiKey Text
  deriving stock (Eq)
  deriving newtype (IsString)

-- | Smart constructor for ApiKey. Returns Nothing for empty keys.
mkApiKey :: Text -> Maybe ApiKey
mkApiKey t
  | T.null (T.strip t) = Nothing
  | otherwise = Just (ApiKey t)

-- | Extract the raw text from an ApiKey.
{-# INLINE unApiKey #-}
unApiKey :: ApiKey -> Text
unApiKey (ApiKey t) = t

instance Show ApiKey where
  show _ = "ApiKey \"<redacted>\""

-- | A model identifier (e.g., "claude-sonnet-4-20250514", "gpt-4o").
newtype ModelId = ModelId Text
  deriving stock (Eq)
  deriving newtype (Show, IsString)

-- | Smart constructor for ModelId. Returns Nothing for empty model names.
mkModelId :: Text -> Maybe ModelId
mkModelId t
  | T.null (T.strip t) = Nothing
  | otherwise = Just (ModelId t)

-- | Extract the raw text from a ModelId.
{-# INLINE unModelId #-}
unModelId :: ModelId -> Text
unModelId (ModelId t) = t

-- | A temperature value for LLM sampling (typically 0.0 to 2.0).
newtype Temperature = Temperature Double
  deriving stock (Eq)
  deriving newtype (Show, Num, Fractional, Ord)

-- | Smart constructor for Temperature. Returns Nothing for out-of-range values.
mkTemperature :: Double -> Maybe Temperature
mkTemperature t
  | t < 0.0 || t > 2.0 = Nothing
  | otherwise = Just (Temperature t)

-- | Extract the raw Double from a Temperature.
{-# INLINE unTemperature #-}
unTemperature :: Temperature -> Double
unTemperature (Temperature t) = t

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Errors that can occur during Clonad operations.
data ClonadError
  = -- | Failed to parse the LLM response into the expected type
    ClonadParseError
      { parseSpec :: Text,
        parseRaw :: Text,
        parseError :: Text,
        parseExpectedType :: Text,
        parseCallStack :: CallStack
      }
  | -- | API request failed
    ClonadApiError
      { apiMessage :: Text,
        apiBackend :: Text,
        apiCallStack :: CallStack
      }
  | -- | Configuration error (missing env vars, invalid values)
    ClonadConfigError
      { configMessage :: Text,
        configCallStack :: CallStack
      }

instance Show ClonadError where
  show (ClonadParseError spec raw err expected cs) =
    unlines
      [ "ClonadParseError:",
        "  parseSpec = " <> show spec,
        "  parseRaw = " <> show (T.take 100 raw) <> if T.length raw > 100 then "..." else "",
        "  parseError = " <> show err,
        "  parseExpectedType = " <> show expected,
        "  parseCallStack = " <> prettyCallStack cs
      ]
  show (ClonadApiError msg backend cs) =
    unlines
      [ "ClonadApiError:",
        "  apiMessage = " <> show msg,
        "  apiBackend = " <> show backend,
        "  apiCallStack = " <> prettyCallStack cs
      ]
  show (ClonadConfigError msg cs) =
    unlines
      [ "ClonadConfigError:",
        "  configMessage = " <> show msg,
        "  configCallStack = " <> prettyCallStack cs
      ]

instance Eq ClonadError where
  ClonadParseError s1 r1 e1 t1 _ == ClonadParseError s2 r2 e2 t2 _ =
    s1 == s2 && r1 == r2 && e1 == e2 && t1 == t2
  ClonadApiError m1 b1 _ == ClonadApiError m2 b2 _ =
    m1 == m2 && b1 == b2
  ClonadConfigError m1 _ == ClonadConfigError m2 _ =
    m1 == m2
  _ == _ = False

instance Exception ClonadError

-- ---------------------------------------------------------------------------
-- Serialisation Typeclasses
-- ---------------------------------------------------------------------------

-- | Types that can be passed as input to a Claude FFI call.
class ClonadParam a where
  serialise :: a -> Text
  default serialise :: (Show a) => a -> Text
  serialise = T.pack . show

-- | Types that can be parsed from Claude's response.
class ClonadReturn a where
  returnSpec :: Proxy a -> Text
  deserialise :: Text -> Either Text a

  default returnSpec :: (Typeable a) => Proxy a -> Text
  returnSpec p = "Return ONLY a valid JSON value of Haskell type: " <> T.pack (show (typeRep p))

  default deserialise :: (FromJSON a) => Text -> Either Text a
  deserialise = parseJsonText

-- ---------------------------------------------------------------------------
-- Serialisation Helpers
-- ---------------------------------------------------------------------------

-- | Serialise a Show instance to Text.
showSerialise :: (Show a) => a -> Text
showSerialise = T.pack . show
{-# INLINE showSerialise #-}

-- | Parse JSON from text, stripping code fences.
parseJsonText :: (FromJSON a) => Text -> Either Text a
parseJsonText = first T.pack . eitherDecodeStrict . TE.encodeUtf8 . stripCodeFences
{-# INLINE parseJsonText #-}

-- ---------------------------------------------------------------------------
-- ClonadParam instances
-- ---------------------------------------------------------------------------

instance ClonadParam Text where serialise = id

instance ClonadParam String where serialise = T.pack

instance ClonadParam Int where serialise = showSerialise

instance ClonadParam Integer where serialise = showSerialise

instance ClonadParam Double where serialise = showSerialise

instance ClonadParam Bool where serialise = showSerialise

instance ClonadParam Float where serialise = showSerialise

instance ClonadParam Word where serialise = showSerialise

instance ClonadParam Word8 where serialise = showSerialise

instance ClonadParam Word16 where serialise = showSerialise

instance ClonadParam Word32 where serialise = showSerialise

instance ClonadParam Word64 where serialise = showSerialise

instance ClonadParam Int8 where serialise = showSerialise

instance ClonadParam Int16 where serialise = showSerialise

instance ClonadParam Int32 where serialise = showSerialise

instance ClonadParam Int64 where serialise = showSerialise

instance ClonadParam Natural where serialise = showSerialise

instance ClonadParam () where serialise = const "()"

instance (ClonadParam a) => ClonadParam [a] where
  serialise = T.intercalate "\n" . map serialise

instance (ClonadParam a, ClonadParam b) => ClonadParam (a, b) where
  serialise (a, b) = "(" <> serialise a <> ", " <> serialise b <> ")"

instance (ClonadParam a, ClonadParam b, ClonadParam c) => ClonadParam (a, b, c) where
  serialise (a, b, c) = "(" <> serialise a <> ", " <> serialise b <> ", " <> serialise c <> ")"

instance (ClonadParam a, ClonadParam b, ClonadParam c, ClonadParam d) => ClonadParam (a, b, c, d) where
  serialise (a, b, c, d) = "(" <> serialise a <> ", " <> serialise b <> ", " <> serialise c <> ", " <> serialise d <> ")"

instance (ClonadParam a, ClonadParam b, ClonadParam c, ClonadParam d, ClonadParam e) => ClonadParam (a, b, c, d, e) where
  serialise (a, b, c, d, e) = "(" <> serialise a <> ", " <> serialise b <> ", " <> serialise c <> ", " <> serialise d <> ", " <> serialise e <> ")"

instance (ClonadParam a) => ClonadParam (NonEmpty a) where
  serialise = T.intercalate "\n" . map serialise . NE.toList

instance (ClonadParam a) => ClonadParam (Set a) where
  serialise = T.intercalate "\n" . map serialise . Set.toList

instance (ClonadParam k, ClonadParam v) => ClonadParam (Map k v) where
  serialise m = T.intercalate "\n" [serialise k <> ": " <> serialise v | (k, v) <- Map.toList m]

instance (ClonadParam a) => ClonadParam (Vector a) where
  serialise = T.intercalate "\n" . map serialise . V.toList

-- ---------------------------------------------------------------------------
-- ClonadReturn instances
-- ---------------------------------------------------------------------------

instance ClonadReturn Text where
  returnSpec _ = "Return ONLY the raw text output. No markdown, no code fences, no explanation."
  deserialise = Right . T.strip

instance ClonadReturn String where
  returnSpec _ = "Return ONLY the raw text output. No markdown, no code fences, no explanation."
  deserialise = Right . T.unpack . T.strip

instance ClonadReturn Int where
  returnSpec _ = "Return ONLY an integer. No text, no explanation."
  deserialise = parseWith @Int "Int"

instance ClonadReturn Integer where
  returnSpec _ = "Return ONLY an integer. No text, no explanation."
  deserialise = parseWith @Integer "Integer"

instance ClonadReturn Double where
  returnSpec _ = "Return ONLY a number. No text, no explanation."
  deserialise = parseWith @Double "Double"

instance ClonadReturn Bool where
  returnSpec _ = "Return ONLY true or false."
  deserialise t = case T.toLower (T.strip t) of
    "true" -> Right True
    "false" -> Right False
    other -> Left $ "Not a Bool: " <> other

instance ClonadReturn Float where
  returnSpec _ = "Return ONLY a number. No text, no explanation."
  deserialise = parseWith @Float "Float"

instance ClonadReturn Word where
  returnSpec _ = "Return ONLY a non-negative integer. No text, no explanation."
  deserialise = parseWith @Word "Word"

instance ClonadReturn Word8 where
  returnSpec _ = "Return ONLY an integer from 0 to 255. No text, no explanation."
  deserialise = parseWith @Word8 "Word8"

instance ClonadReturn Word16 where
  returnSpec _ = "Return ONLY a non-negative integer. No text, no explanation."
  deserialise = parseWith @Word16 "Word16"

instance ClonadReturn Word32 where
  returnSpec _ = "Return ONLY a non-negative integer. No text, no explanation."
  deserialise = parseWith @Word32 "Word32"

instance ClonadReturn Word64 where
  returnSpec _ = "Return ONLY a non-negative integer. No text, no explanation."
  deserialise = parseWith @Word64 "Word64"

instance ClonadReturn Int8 where
  returnSpec _ = "Return ONLY an integer from -128 to 127. No text, no explanation."
  deserialise = parseWith @Int8 "Int8"

instance ClonadReturn Int16 where
  returnSpec _ = "Return ONLY an integer. No text, no explanation."
  deserialise = parseWith @Int16 "Int16"

instance ClonadReturn Int32 where
  returnSpec _ = "Return ONLY an integer. No text, no explanation."
  deserialise = parseWith @Int32 "Int32"

instance ClonadReturn Int64 where
  returnSpec _ = "Return ONLY an integer. No text, no explanation."
  deserialise = parseWith @Int64 "Int64"

instance ClonadReturn Natural where
  returnSpec _ = "Return ONLY a non-negative integer. No text, no explanation."
  deserialise = parseWith @Natural "Natural"

instance ClonadReturn () where
  returnSpec _ = "Do not return any value."
  deserialise = const (Right ())

instance (ClonadReturn a) => ClonadReturn (Maybe a) where
  returnSpec _ = "Return ONLY a JSON value or null. No markdown, no explanation."
  deserialise t =
    let stripped = T.strip t
     in if stripped == "null" || stripped == "Nothing"
          then Right Nothing
          else Just <$> deserialise stripped

instance (FromJSON a) => ClonadReturn [a] where
  returnSpec _ = "Return ONLY a JSON array. No markdown, no code fences, no explanation."
  deserialise = parseJsonText

instance (FromJSON a) => ClonadReturn (NonEmpty a) where
  returnSpec _ = "Return ONLY a non-empty JSON array. No markdown, no code fences, no explanation."
  deserialise t = do
    xs <- parseJsonText t
    maybe (Left "Expected non-empty array") Right $ NE.nonEmpty xs

instance (Ord a, FromJSON a) => ClonadReturn (Set a) where
  returnSpec _ = "Return ONLY a JSON array. No markdown, no code fences, no explanation."
  deserialise t = Set.fromList <$> parseJsonText t

instance (Ord k, Aeson.FromJSONKey k, FromJSON v) => ClonadReturn (Map k v) where
  returnSpec _ = "Return ONLY a JSON object. No markdown, no code fences, no explanation."
  deserialise = parseJsonText

instance (FromJSON a) => ClonadReturn (Vector a) where
  returnSpec _ = "Return ONLY a JSON array. No markdown, no code fences, no explanation."
  deserialise t = V.fromList <$> parseJsonText t

instance (FromJSON a, FromJSON b) => ClonadReturn (a, b) where
  returnSpec _ = "Return ONLY a JSON array of exactly two elements. No markdown, no explanation."
  deserialise t =
    parseJsonText @[Aeson.Value] t >>= \case
      [a, b] -> first T.pack $ (,) <$> parseEither parseJSON a <*> parseEither parseJSON b
      _ -> Left "Expected a two-element array"

instance (FromJSON a, FromJSON b, FromJSON c) => ClonadReturn (a, b, c) where
  returnSpec _ = "Return ONLY a JSON array of exactly three elements. No markdown, no explanation."
  deserialise t =
    parseJsonText @[Aeson.Value] t >>= \case
      [a, b, c] -> first T.pack $ (,,) <$> parseEither parseJSON a <*> parseEither parseJSON b <*> parseEither parseJSON c
      _ -> Left "Expected a three-element array"

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => ClonadReturn (a, b, c, d) where
  returnSpec _ = "Return ONLY a JSON array of exactly four elements. No markdown, no explanation."
  deserialise t =
    parseJsonText @[Aeson.Value] t >>= \case
      [a, b, c, d] -> first T.pack $ (,,,) <$> parseEither parseJSON a <*> parseEither parseJSON b <*> parseEither parseJSON c <*> parseEither parseJSON d
      _ -> Left "Expected a four-element array"

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e) => ClonadReturn (a, b, c, d, e) where
  returnSpec _ = "Return ONLY a JSON array of exactly five elements. No markdown, no explanation."
  deserialise t =
    parseJsonText @[Aeson.Value] t >>= \case
      [a, b, c, d, e] -> first T.pack $ (,,,,) <$> parseEither parseJSON a <*> parseEither parseJSON b <*> parseEither parseJSON c <*> parseEither parseJSON d <*> parseEither parseJSON e
      _ -> Left "Expected a five-element array"

instance (ClonadReturn a, ClonadReturn b) => ClonadReturn (Either a b) where
  returnSpec _ =
    "Return ONLY a JSON object with either {\"Left\": <value>} or {\"Right\": <value>}. No explanation."
  deserialise t =
    parseJsonText @Aeson.Value t >>= \case
      Aeson.Object obj -> case (KM.lookup "Left" obj, KM.lookup "Right" obj) of
        (Just val, Nothing) -> Prelude.Left <$> deserialiseValue val
        (Nothing, Just val) -> Prelude.Right <$> deserialiseValue val
        _ -> Prelude.Left "Expected object with exactly one of 'Left' or 'Right' key"
      _ -> Prelude.Left "Expected a JSON object"
    where
      deserialiseValue :: (ClonadReturn c) => Aeson.Value -> Either Text c
      deserialiseValue v = deserialise (TE.decodeUtf8 . LBS.toStrict $ Aeson.encode v)

-- | Helper for Read-based parsing
parseWith :: forall a. (Read a) => Text -> Text -> Either Text a
parseWith typeName t =
  maybe (Left $ "Not a " <> typeName <> ": " <> t) Right $
    readMaybe (T.unpack $ T.strip t)

-- ---------------------------------------------------------------------------
-- DerivingVia Wrappers
-- ---------------------------------------------------------------------------

-- | Newtype wrapper for deriving 'ClonadParam' via JSON serialisation.
--
-- @
-- data MyType = MyType { field1 :: Int, field2 :: Text }
--   deriving stock (Generic)
--   deriving anyclass (ToJSON)
--   deriving ClonadParam via (AsJSON MyType)
-- @
newtype AsJSON a = AsJSON {unAsJSON :: a}

instance (Aeson.ToJSON a) => ClonadParam (AsJSON a) where
  serialise (AsJSON x) = TE.decodeUtf8 . LBS.toStrict $ Aeson.encode x

-- | Newtype wrapper for deriving 'ClonadParam' via Show.
--
-- @
-- newtype UserId = UserId Int
--   deriving stock (Show)
--   deriving ClonadParam via (ViaShow UserId)
-- @
newtype ViaShow a = ViaShow {unViaShow :: a}

instance (Show a) => ClonadParam (ViaShow a) where
  serialise (ViaShow x) = T.pack (show x)

-- | Newtype wrapper for deriving 'ClonadReturn' via Read.
--
-- @
-- newtype UserId = UserId Int
--   deriving stock (Read, Typeable)
--   deriving ClonadReturn via (ViaRead UserId)
-- @
newtype ViaRead a = ViaRead {unViaRead :: a}

instance (Read a, Typeable a) => ClonadReturn (ViaRead a) where
  returnSpec p = "Return ONLY a valid Haskell value of type: " <> T.pack (show (typeRep p))
  deserialise t =
    maybe (Left $ "Parse failed: " <> t) (Right . ViaRead) $
      readMaybe (T.unpack $ T.strip t)

-- ---------------------------------------------------------------------------
-- The Clonad Monad
-- ---------------------------------------------------------------------------

-- | A computation backed by the Claude API.
--
-- Under the hood this is @ReaderT ClonadEnv IO@. Each 'clonad' call
-- within a pipeline makes a real API request where Claude computes the
-- result described by your natural-language specification.
newtype Clonad a = Clonad (ReaderT ClonadEnv IO a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader ClonadEnv,
      MonadThrow,
      MonadCatch
    )

-- | Constraint alias for polymorphic Clonad operations.
-- Use this when writing functions that should work in any monad
-- with access to 'ClonadEnv'.
type MonadClonad m = (MonadReader ClonadEnv m, MonadIO m, MonadThrow m)

-- | Backend selection for the API.
-- API keys are stored in the backend itself, ensuring type-level correctness
-- (e.g., Ollama doesn't require a key, so it doesn't have one).
data Backend
  = -- | Use Claude via api.anthropic.com (requires API key)
    Anthropic ApiKey
  | -- | Use Ollama at the given base URL (e.g., "http://localhost:11434")
    Ollama Text
  | -- | Use OpenAI at api.openai.com or a custom OpenAI-compatible endpoint
    OpenAI
      ApiKey
      -- | Optional custom base URL; Nothing means api.openai.com
      (Maybe Text)
  deriving stock (Eq)

instance Show Backend where
  show (Anthropic _) = "Anthropic <key>"
  show (Ollama url) = "Ollama " <> show url
  show (OpenAI _ mUrl) = "OpenAI <key> " <> show mUrl

-- | Get display name for a backend (used in error messages).
backendName :: Backend -> Text
backendName (Anthropic {}) = "Anthropic"
backendName (Ollama {}) = "Ollama"
backendName (OpenAI {}) = "OpenAI"

-- | The execution environment.
data ClonadEnv = ClonadEnv
  { backend :: Backend,
    model :: ModelId,
    temperature :: Maybe Temperature,
    maxTokens :: Int,
    systemPrompt :: Maybe Text
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Environment Construction
-- ---------------------------------------------------------------------------

-- | Read environment variables and construct a 'ClonadEnv'.
-- Checks @OLLAMA_HOST@ first (for local Ollama), then @OPENAI_API_KEY@, then @ANTHROPIC_API_KEY@.
-- Throws 'ClonadConfigError' if no valid configuration is found.
defaultEnv :: (HasCallStack) => IO ClonadEnv
defaultEnv = do
  mOllama <- lookupEnv "OLLAMA_HOST"
  mModel <- lookupEnv "CLONAD_MODEL"
  mOpenAIBase <- lookupEnv "OPENAI_BASE_URL"
  case mOllama of
    Just url ->
      let modelId = maybe (ModelId "qwen2.5:0.5b") (ModelId . T.pack) mModel
       in pure $ mkOllamaEnv (T.pack url) modelId
    Nothing ->
      lookupEnv "OPENAI_API_KEY" >>= \case
        Just key ->
          let modelId = maybe (ModelId "gpt-4o-mini") (ModelId . T.pack) mModel
           in case mkApiKey (T.pack key) of
                Nothing -> throwClonadConfigError "OPENAI_API_KEY is empty"
                Just apiKey -> pure $ mkOpenAIEnv apiKey modelId (T.pack <$> mOpenAIBase)
        Nothing ->
          lookupEnv "ANTHROPIC_API_KEY" >>= \case
            Nothing ->
              throwClonadConfigError
                "none of OLLAMA_HOST, OPENAI_API_KEY, or ANTHROPIC_API_KEY set"
            Just key -> case mkApiKey (T.pack key) of
              Nothing -> throwClonadConfigError "ANTHROPIC_API_KEY is empty"
              Just apiKey -> pure $ mkEnv apiKey

-- | Throw a ClonadConfigError with the current call stack.
throwClonadConfigError :: (HasCallStack) => Text -> IO a
throwClonadConfigError msg =
  throwIO $ ClonadConfigError {configMessage = msg, configCallStack = callStack}

-- | Construct a 'ClonadEnv' for Anthropic.
mkEnv :: ApiKey -> ClonadEnv
mkEnv key =
  ClonadEnv
    { backend = Anthropic key,
      model = ModelId "claude-sonnet-4-20250514",
      temperature = Nothing,
      maxTokens = 1024,
      systemPrompt = Nothing
    }

-- | Construct a 'ClonadEnv' for Ollama.
mkOllamaEnv :: Text -> ModelId -> ClonadEnv
mkOllamaEnv baseUrl modelId =
  ClonadEnv
    { backend = Ollama baseUrl,
      model = modelId,
      temperature = Nothing,
      maxTokens = 1024,
      systemPrompt = Nothing
    }

-- | Construct a 'ClonadEnv' for OpenAI or an OpenAI-compatible endpoint.
mkOpenAIEnv :: ApiKey -> ModelId -> Maybe Text -> ClonadEnv
mkOpenAIEnv key modelId mBaseUrl =
  ClonadEnv
    { backend = OpenAI key mBaseUrl,
      model = modelId,
      temperature = Nothing,
      maxTokens = 1024,
      systemPrompt = Nothing
    }

-- ---------------------------------------------------------------------------
-- Runner
-- ---------------------------------------------------------------------------

-- | Run a 'Clonad' computation.
{-# INLINE runClonad #-}
runClonad :: ClonadEnv -> Clonad a -> IO a
runClonad env (Clonad m) = runReaderT m env

-- | Run a 'Clonad' computation, catching 'ClonadError' exceptions.
runClonadEither :: ClonadEnv -> Clonad a -> IO (Either ClonadError a)
runClonadEither env action = try @ClonadError $ runClonad env action

-- ---------------------------------------------------------------------------
-- The Claude FFI
-- ---------------------------------------------------------------------------

-- | Define a function whose implementation is Claude.
--
-- @clonad@ takes a natural-language specification and returns a function
-- from any 'ClonadParam' to any 'ClonadReturn'. The Haskell type
-- signature constrains what Claude must return.
--
-- @
-- sentiment :: Text -> Clonad Double
-- sentiment = clonad "return a sentiment score from -1.0 to 1.0"
--
-- translate :: Text -> Clonad Text
-- translate = clonad "translate this to French"
--
-- isSpam :: Text -> Clonad Bool
-- isSpam = clonad "is this email spam?"
-- @
clonad :: forall a b. (ClonadParam a, ClonadReturn b, Typeable b, HasCallStack) => Text -> a -> Clonad b
clonad = clonadImpl

-- | Like 'clonad' but takes no input. Useful for generation tasks.
--
-- @
-- poem :: Clonad Text
-- poem = clonad_ "write a haiku about Haskell"
-- @
clonad_ :: forall b. (ClonadReturn b, Typeable b, HasCallStack) => Text -> Clonad b
clonad_ spec = clonadImpl spec ()

-- | Like 'clonad' but returns 'Either' instead of throwing.
--
-- @
-- sentiment :: Text -> Clonad (Either ClonadError Double)
-- sentiment = tryClonad "return a sentiment score from -1.0 to 1.0"
-- @
tryClonad ::
  forall a b.
  (ClonadParam a, ClonadReturn b, Typeable b, HasCallStack) =>
  Text ->
  a ->
  Clonad (Either ClonadError b)
tryClonad spec input = Clonad $ ReaderT $ \env ->
  try @ClonadError $ runClonad env (clonadImpl spec input)

-- | Like 'clonad_' but returns 'Either' instead of throwing.
tryClonad_ ::
  forall b.
  (ClonadReturn b, Typeable b, HasCallStack) =>
  Text ->
  Clonad (Either ClonadError b)
tryClonad_ spec = tryClonad spec ()

-- | Internal implementation of the Claude FFI.
clonadImpl ::
  forall a b.
  (ClonadParam a, ClonadReturn b, Typeable b, HasCallStack) =>
  Text ->
  a ->
  Clonad b
clonadImpl spec input = do
  env <- ask
  let rspec = returnSpec (Proxy @b)
      serialised = serialise input
      systemMsg =
        T.unlines $
          catMaybes
            [ env.systemPrompt,
              Just $
                T.unlines
                  [ "You are a pure computation engine embedded in a Haskell program via FFI.",
                    "You receive an input and a specification.",
                    "You return ONLY the computed result.",
                    "No explanation. No preamble. No apologies. No markdown formatting unless the specification requires it.",
                    "",
                    "OUTPUT FORMAT: " <> rspec
                  ]
            ]
      userMsg
        | serialised == "()" = spec
        | otherwise = spec <> "\n\nInput:\n" <> serialised
  rawText <- liftIO $ callLLM env systemMsg userMsg
  case deserialise @b rawText of
    Left err ->
      throwM $
        ClonadParseError
          { parseSpec = spec,
            parseRaw = rawText,
            parseError = err,
            parseExpectedType = T.pack $ show (typeRep (Proxy @b)),
            parseCallStack = callStack
          }
    Right val -> pure val

-- ---------------------------------------------------------------------------
-- HTTP Layer
-- ---------------------------------------------------------------------------

-- Anthropic response types
newtype ApiResponse = ApiResponse {blocks :: NonEmpty ContentBlock}

data ContentBlock = ContentBlock
  { blockType :: Text,
    blockText :: Text
  }

instance FromJSON ApiResponse where
  parseJSON = Aeson.withObject "ApiResponse" \o -> do
    content <- o .: "content"
    case NE.nonEmpty content of
      Nothing -> fail "Expected at least one content block"
      Just ne -> pure $ ApiResponse ne

instance FromJSON ContentBlock where
  parseJSON = Aeson.withObject "ContentBlock" \o ->
    ContentBlock <$> o .: "type" <*> o .: "text"

-- OpenAI/Ollama response types
newtype ChatResponse = ChatResponse {choices :: NonEmpty ChatChoice}

newtype ChatChoice = ChatChoice {message :: ChatMessage}

newtype ChatMessage = ChatMessage {content :: Text}

instance FromJSON ChatResponse where
  parseJSON = Aeson.withObject "ChatResponse" \o -> do
    choices <- o .: "choices"
    case NE.nonEmpty choices of
      Nothing -> fail "Expected at least one choice"
      Just ne -> pure $ ChatResponse ne

instance FromJSON ChatChoice where
  parseJSON = Aeson.withObject "ChatChoice" \o -> ChatChoice <$> o .: "message"

instance FromJSON ChatMessage where
  parseJSON = Aeson.withObject "ChatMessage" \o -> ChatMessage <$> o .: "content"

-- ---------------------------------------------------------------------------
-- URL Parsing Helper
-- ---------------------------------------------------------------------------

-- | Parsed URL components for HTTP requests.
data ParsedUrl = ParsedUrl
  { scheme :: UrlScheme,
    host :: Text,
    portNum :: Int
  }

-- | URL scheme (HTTP or HTTPS).
data UrlScheme = UrlHttp | UrlHttps
  deriving stock (Eq)

-- | A URL ready for req, abstracting over scheme.
data ReqUrl where
  ReqUrlHttps :: Url 'Https -> ReqUrl
  ReqUrlHttp :: Url 'Http -> Int -> ReqUrl

-- | Build URL from parsed components and path segments.
buildReqUrl :: ParsedUrl -> [Text] -> ReqUrl
buildReqUrl parsed segments =
  let addPath url [] = url
      addPath url (p : ps) = addPath (url /: p) ps
   in case parsed.scheme of
        UrlHttps -> ReqUrlHttps (addPath (https parsed.host) segments)
        UrlHttp -> ReqUrlHttp (addPath (http parsed.host) segments) parsed.portNum

-- | Execute POST with JSON body, dispatching on scheme.
runJsonPost ::
  ReqUrl ->
  Aeson.Value ->
  Option 'Https ->
  Option 'Http ->
  IO LBS.ByteString
runJsonPost url body httpsOpts httpOpts = runReq defaultHttpConfig $ case url of
  ReqUrlHttps u -> responseBody <$> req POST u (ReqBodyJson body) lbsResponse httpsOpts
  ReqUrlHttp u p -> responseBody <$> req POST u (ReqBodyJson body) lbsResponse (httpOpts <> port p)

-- | Parse a base URL into its components.
-- Returns Left with an error message if the URL is invalid.
parseBaseUrl :: Text -> Int -> Either Text ParsedUrl
parseBaseUrl baseUrl defaultPort = do
  let stripped = T.dropWhileEnd (== '/') baseUrl
  unless ("://" `T.isInfixOf` stripped) $
    Left "Invalid URL: missing scheme (http:// or https://)"
  let (schemeText, rest) = T.breakOn "://" stripped
      hostPort' = T.drop 3 rest
      (hostText, portPart) = T.breakOn ":" hostPort'
  when (T.null hostText) $
    Left "Invalid URL: empty host"
  let parsedPort = fromMaybe defaultPort $ readMaybe (T.unpack $ T.drop 1 portPart)
      urlScheme = if schemeText == "https" then UrlHttps else UrlHttp
  pure
    ParsedUrl
      { scheme = urlScheme,
        host = hostText,
        portNum = parsedPort
      }

-- ---------------------------------------------------------------------------
-- API Constants
-- ---------------------------------------------------------------------------

anthropicHost :: Text
anthropicHost = "api.anthropic.com"

openaiHost :: Text
openaiHost = "api.openai.com"

chatCompletionsPath :: [Text]
chatCompletionsPath = ["v1", "chat", "completions"]

anthropicVersionHeader :: ByteString
anthropicVersionHeader = "2023-06-01"

ollamaDefaultPort :: Int
ollamaDefaultPort = 11434

httpsDefaultPort :: Int
httpsDefaultPort = 443

-- ---------------------------------------------------------------------------
-- Backend Implementations
-- ---------------------------------------------------------------------------

-- | Standard JSON content-type header.
jsonContentType :: Option scheme
jsonContentType = header "content-type" "application/json"

-- | Bearer token authentication header.
bearerAuth :: ApiKey -> Option scheme
bearerAuth key = header "Authorization" ("Bearer " <> TE.encodeUtf8 (unApiKey key))

-- | Build chat messages for OpenAI-compatible APIs.
buildChatMessages :: Text -> Text -> [Aeson.Value]
buildChatMessages systemMsg userMsg =
  [ object ["role" .= ("system" :: Text), "content" .= systemMsg],
    object ["role" .= ("user" :: Text), "content" .= userMsg]
  ]

-- | Build request body for OpenAI-compatible chat completion APIs.
buildChatBody :: ModelId -> [Aeson.Value] -> Maybe Temperature -> Aeson.Value
buildChatBody modelId messages mTemp =
  object $
    ["model" .= unModelId modelId, "messages" .= messages]
      <> ["temperature" .= unTemperature t | Just t <- [mTemp]]

-- | Call OpenAI-compatible chat completion endpoint with scheme dispatch.
-- Headers are provided per-scheme to handle type differences.
callChatCompletion ::
  ParsedUrl ->
  Option 'Https ->
  Option 'Http ->
  ClonadEnv ->
  Text ->
  Text ->
  IO Text
callChatCompletion parsed httpsHeaders httpHeaders env systemMsg userMsg = do
  let messages = buildChatMessages systemMsg userMsg
      body = buildChatBody env.model messages env.temperature
      url = buildReqUrl parsed chatCompletionsPath
  respBytes <- runJsonPost url body (httpsHeaders <> jsonContentType) (httpHeaders <> jsonContentType)
  case eitherDecodeStrict (LBS.toStrict respBytes) of
    Left err -> fail $ "Failed to parse chat response: " <> err
    Right resp -> pure $ extractChatContent resp

-- | Call the LLM backend, wrapping HTTP exceptions in ClonadApiError.
callLLM :: (HasCallStack) => ClonadEnv -> Text -> Text -> IO Text
callLLM env systemMsg userMsg = do
  let backend = backendName env.backend
  result <- try @SomeException $ case env.backend of
    Anthropic key -> callAnthropic env key systemMsg userMsg
    Ollama base -> callOllama env base systemMsg userMsg
    OpenAI key mBase -> callOpenAI env key mBase systemMsg userMsg
  case result of
    Left err ->
      throwIO $
        ClonadApiError
          { apiMessage = T.pack $ show err,
            apiBackend = backend,
            apiCallStack = callStack
          }
    Right txt -> pure txt

callAnthropic :: ClonadEnv -> ApiKey -> Text -> Text -> IO Text
callAnthropic env apiKey systemMsg userMsg = runReq defaultHttpConfig do
  let url = https anthropicHost /: "v1" /: "messages"
      headers =
        header "x-api-key" (TE.encodeUtf8 $ unApiKey apiKey)
          <> header "anthropic-version" anthropicVersionHeader
          <> jsonContentType
      body =
        object $
          [ "model" .= unModelId env.model,
            "max_tokens" .= env.maxTokens,
            "system" .= systemMsg,
            "messages" .= [object ["role" .= ("user" :: Text), "content" .= userMsg]]
          ]
            <> ["temperature" .= unTemperature t | Just t <- [env.temperature]]
  resp <- req POST url (ReqBodyJson body) jsonResponse headers
  let ApiResponse contentBlocks = responseBody resp
  pure $ T.intercalate "\n" [blk.blockText | blk <- NE.toList contentBlocks, blk.blockType == "text"]

callOllama :: (HasCallStack) => ClonadEnv -> Text -> Text -> Text -> IO Text
callOllama env baseUrl systemMsg userMsg = do
  parsed <- either (throwClonadApiError "Ollama") pure $ parseBaseUrl baseUrl ollamaDefaultPort
  callChatCompletion parsed mempty mempty env systemMsg userMsg

callOpenAI :: (HasCallStack) => ClonadEnv -> ApiKey -> Maybe Text -> Text -> Text -> IO Text
callOpenAI env apiKey mBaseUrl systemMsg userMsg = do
  parsed <- case mBaseUrl of
    Nothing -> pure ParsedUrl {scheme = UrlHttps, host = openaiHost, portNum = httpsDefaultPort}
    Just url -> either (throwClonadApiError "OpenAI") pure $ parseBaseUrl url httpsDefaultPort
  callChatCompletion parsed (bearerAuth apiKey) (bearerAuth apiKey) env systemMsg userMsg

-- | Throw a ClonadApiError with the current call stack.
throwClonadApiError :: (HasCallStack) => Text -> Text -> IO a
throwClonadApiError backend msg =
  throwIO $ ClonadApiError {apiMessage = msg, apiBackend = backend, apiCallStack = callStack}

-- | Extract text content from a chat completion response.
extractChatContent :: ChatResponse -> Text
extractChatContent resp = case resp.choices of
  (choice :| _) -> choice.message.content

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Strip markdown code fences from LLM output.
stripCodeFences :: Text -> Text
stripCodeFences t = case T.lines (T.strip t) of
  (firstLine : rest)
    | "```" `T.isPrefixOf` firstLine -> T.strip . T.unlines $ dropTrailingFence rest
  other -> T.unlines other
  where
    -- Drop trailing fence if present, using efficient reverse-based approach
    dropTrailingFence [] = []
    dropTrailingFence xs = case reverse xs of
      (lastLine : body)
        | "```" `T.isPrefixOf` lastLine -> reverse body
      _ -> xs

-- ---------------------------------------------------------------------------
-- Combinators
-- ---------------------------------------------------------------------------

-- | Locally override the model.
withModel :: ModelId -> Clonad a -> Clonad a
withModel m = local \e -> e {model = m}

-- | Locally override the temperature.
withTemperature :: Temperature -> Clonad a -> Clonad a
withTemperature t = local \e -> e {temperature = Just t}

-- | Locally set a system prompt.
withSystemPrompt :: Text -> Clonad a -> Clonad a
withSystemPrompt s = local \e -> e {systemPrompt = Just s}
