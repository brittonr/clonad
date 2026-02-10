{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

    -- * Serialisation
    ClonadParam (..),
    ClonadReturn (..),
    AsJSON (..),

    -- * Environment
    defaultEnv,
    mkEnv,
    mkOllamaEnv,
    mkOpenAIEnv,

    -- * Runner
    runClonad,

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
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Typeable (Typeable, typeRep)
import Network.HTTP.Req
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
        parseExpectedType :: Text
      }
  | -- | API request failed
    ClonadApiError
      { apiMessage :: Text,
        apiBackend :: Text
      }
  | -- | Configuration error (missing env vars, invalid values)
    ClonadConfigError
      { configMessage :: Text
      }
  deriving stock (Show, Eq)

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
  deserialise = first T.pack . eitherDecodeStrict . TE.encodeUtf8 . stripCodeFences

-- ---------------------------------------------------------------------------
-- ClonadParam instances
-- ---------------------------------------------------------------------------

instance ClonadParam Text where serialise = id

instance ClonadParam String where serialise = T.pack

instance ClonadParam Int where serialise = T.pack . show

instance ClonadParam Integer where serialise = T.pack . show

instance ClonadParam Double where serialise = T.pack . show

instance ClonadParam Bool where serialise = T.pack . show

instance ClonadParam () where serialise = const "()"

instance (ClonadParam a) => ClonadParam [a] where
  serialise = T.intercalate "\n" . map serialise

instance (ClonadParam a, ClonadParam b) => ClonadParam (a, b) where
  serialise (a, b) = "(" <> serialise a <> ", " <> serialise b <> ")"

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
  deserialise = first T.pack . eitherDecodeStrict . TE.encodeUtf8 . stripCodeFences

instance (FromJSON a, FromJSON b) => ClonadReturn (a, b) where
  returnSpec _ = "Return ONLY a JSON array of exactly two elements. No markdown, no explanation."
  deserialise t =
    first T.pack (eitherDecodeStrict (TE.encodeUtf8 $ stripCodeFences t)) >>= \case
      [a, b] -> first T.pack $ (,) <$> parseEither parseJSON a <*> parseEither parseJSON b
      _ -> Left "Expected a two-element array"

instance (ClonadReturn a, ClonadReturn b) => ClonadReturn (Either a b) where
  returnSpec _ =
    "Return ONLY a JSON object with either {\"Left\": <value>} or {\"Right\": <value>}. No explanation."
  deserialise t =
    let stripped = stripCodeFences t
     in case first T.pack (eitherDecodeStrict @Aeson.Value (TE.encodeUtf8 stripped)) of
          Prelude.Left err -> Prelude.Left err
          Prelude.Right (Aeson.Object obj) -> case (KM.lookup "Left" obj, KM.lookup "Right" obj) of
            (Just val, Nothing) -> Prelude.Left <$> deserialiseValue val
            (Nothing, Just val) -> Prelude.Right <$> deserialiseValue val
            _ -> Prelude.Left "Expected object with exactly one of 'Left' or 'Right' key"
          Prelude.Right _ -> Prelude.Left "Expected a JSON object"
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
defaultEnv :: IO ClonadEnv
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
                Nothing -> throwIO $ ClonadConfigError "OPENAI_API_KEY is empty"
                Just apiKey -> pure $ mkOpenAIEnv apiKey modelId (T.pack <$> mOpenAIBase)
        Nothing ->
          lookupEnv "ANTHROPIC_API_KEY" >>= \case
            Nothing ->
              throwIO $
                ClonadConfigError
                  "none of OLLAMA_HOST, OPENAI_API_KEY, or ANTHROPIC_API_KEY set"
            Just key -> case mkApiKey (T.pack key) of
              Nothing -> throwIO $ ClonadConfigError "ANTHROPIC_API_KEY is empty"
              Just apiKey -> pure $ mkEnv apiKey

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
clonad :: forall a b. (ClonadParam a, ClonadReturn b, Typeable b) => Text -> a -> Clonad b
clonad = clonadImpl

-- | Like 'clonad' but takes no input. Useful for generation tasks.
--
-- @
-- poem :: Clonad Text
-- poem = clonad_ "write a haiku about Haskell"
-- @
clonad_ :: forall b. (ClonadReturn b, Typeable b) => Text -> Clonad b
clonad_ spec = clonadImpl spec ()

-- | Internal implementation of the Claude FFI.
clonadImpl ::
  forall a b.
  (ClonadParam a, ClonadReturn b, Typeable b) =>
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
            parseExpectedType = T.pack $ show (typeRep (Proxy @b))
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
-- Backend Implementations
-- ---------------------------------------------------------------------------

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

-- | Call the LLM backend, wrapping HTTP exceptions in ClonadApiError.
callLLM :: ClonadEnv -> Text -> Text -> IO Text
callLLM env systemMsg userMsg = do
  let backendName = case env.backend of
        Anthropic {} -> "Anthropic"
        Ollama {} -> "Ollama"
        OpenAI {} -> "OpenAI"
  result <- try @SomeException $ case env.backend of
    Anthropic key -> callAnthropic env key systemMsg userMsg
    Ollama base -> callOllama env base systemMsg userMsg
    OpenAI key mBase -> callOpenAI env key mBase systemMsg userMsg
  case result of
    Left err -> throwIO $ ClonadApiError {apiMessage = T.pack $ show err, apiBackend = backendName}
    Right txt -> pure txt

callAnthropic :: ClonadEnv -> ApiKey -> Text -> Text -> IO Text
callAnthropic env apiKey systemMsg userMsg = runReq defaultHttpConfig do
  let url = https "api.anthropic.com" /: "v1" /: "messages"
      headers =
        header "x-api-key" (TE.encodeUtf8 $ unApiKey apiKey)
          <> header "anthropic-version" "2023-06-01"
          <> header "content-type" "application/json"
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

callOllama :: ClonadEnv -> Text -> Text -> Text -> IO Text
callOllama env baseUrl systemMsg userMsg = do
  parsed <- case parseBaseUrl baseUrl 11434 of
    Left err -> throwIO $ ClonadApiError {apiMessage = err, apiBackend = "Ollama"}
    Right p -> pure p
  runReq defaultHttpConfig do
    let messages = buildChatMessages systemMsg userMsg
        body = buildChatBody env.model messages env.temperature
        headers = header "content-type" "application/json"
    resp <- case parsed.scheme of
      UrlHttps ->
        req POST (https parsed.host /: "v1" /: "chat" /: "completions") (ReqBodyJson body) jsonResponse headers
      UrlHttp ->
        req POST (http parsed.host /: "v1" /: "chat" /: "completions") (ReqBodyJson body) jsonResponse (headers <> port parsed.portNum)
    pure $ extractChatContent (responseBody resp)

callOpenAI :: ClonadEnv -> ApiKey -> Maybe Text -> Text -> Text -> IO Text
callOpenAI env apiKey mBaseUrl systemMsg userMsg = do
  mParsed <- case mBaseUrl of
    Nothing -> pure Nothing
    Just baseUrl -> case parseBaseUrl baseUrl 443 of
      Left err -> throwIO $ ClonadApiError {apiMessage = err, apiBackend = "OpenAI"}
      Right p -> pure $ Just p
  runReq defaultHttpConfig do
    let messages = buildChatMessages systemMsg userMsg
        body = buildChatBody env.model messages env.temperature
        authHeader = header "Authorization" ("Bearer " <> TE.encodeUtf8 (unApiKey apiKey))
        headers = authHeader <> header "content-type" "application/json"
    resp <- case mParsed of
      Nothing ->
        -- Default OpenAI endpoint
        req POST (https "api.openai.com" /: "v1" /: "chat" /: "completions") (ReqBodyJson body) jsonResponse headers
      Just parsed ->
        -- Custom OpenAI-compatible endpoint
        case parsed.scheme of
          UrlHttps ->
            req POST (https parsed.host /: "v1" /: "chat" /: "completions") (ReqBodyJson body) jsonResponse headers
          UrlHttp ->
            req POST (http parsed.host /: "v1" /: "chat" /: "completions") (ReqBodyJson body) jsonResponse (headers <> port parsed.portNum)
    pure $ extractChatContent (responseBody resp)

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
