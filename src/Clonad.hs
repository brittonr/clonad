{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
    ClonadEnv (..),
    Backend (..),

    -- * The Claude FFI
    clonad,
    clonad_,
    clonadWith,

    -- * Serialisation
    ClonadParam (..),
    ClonadReturn (..),

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

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), local)
import Data.Aeson (FromJSON (..), eitherDecodeStrict, object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Typeable (Typeable, typeRep)
import Network.HTTP.Req
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

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
  deserialise :: Text -> Either String a

  default returnSpec :: (Typeable a) => Proxy a -> Text
  returnSpec p = "Return ONLY a valid JSON value of Haskell type: " <> T.pack (show (typeRep p))

  default deserialise :: (FromJSON a) => Text -> Either String a
  deserialise = eitherDecodeStrict . TE.encodeUtf8 . stripCodeFences

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
    other -> Left $ "Not a Bool: " <> T.unpack other

instance ClonadReturn () where
  returnSpec _ = "Do not return any value."
  deserialise = const (Right ())

instance (FromJSON a) => ClonadReturn [a] where
  returnSpec _ = "Return ONLY a JSON array. No markdown, no code fences, no explanation."
  deserialise = eitherDecodeStrict . TE.encodeUtf8 . stripCodeFences

instance (FromJSON a, FromJSON b) => ClonadReturn (a, b) where
  returnSpec _ = "Return ONLY a JSON array of exactly two elements. No markdown, no explanation."
  deserialise t =
    eitherDecodeStrict (TE.encodeUtf8 $ stripCodeFences t) >>= \case
      [a, b] -> (,) <$> parseEither parseJSON a <*> parseEither parseJSON b
      _ -> Left "Expected a two-element array"

-- | Helper for Read-based parsing
parseWith :: forall a. (Read a) => String -> Text -> Either String a
parseWith typeName t =
  maybe (Left $ "Not a " <> typeName <> ": " <> show t) Right $
    readMaybe (T.unpack $ T.strip t)

-- ---------------------------------------------------------------------------
-- The Clonad Monad
-- ---------------------------------------------------------------------------

-- | A computation backed by the Claude API.
--
-- Under the hood this is @ReaderT ClonadEnv IO@. Each 'clonad' call
-- within a pipeline makes a real API request where Claude computes the
-- result described by your natural-language specification.
newtype Clonad a = Clonad {unClonad :: ReaderT ClonadEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader ClonadEnv)

-- | Backend selection for the API
data Backend
  = -- | Use Claude via api.anthropic.com
    Anthropic
  | -- | Use Ollama at the given base URL (e.g., "http://localhost:11434")
    Ollama Text
  | -- | Use OpenAI at api.openai.com or a custom OpenAI-compatible endpoint
    OpenAI
      -- | Optional custom base URL; Nothing means api.openai.com
      (Maybe Text)
  deriving stock (Show, Eq)

-- | The execution environment.
data ClonadEnv = ClonadEnv
  { envBackend :: !Backend,
    envApiKey :: !Text,
    envModel :: !Text,
    envTemperature :: !(Maybe Double),
    envMaxTokens :: !Int,
    envSystemPrompt :: !(Maybe Text)
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Environment Construction
-- ---------------------------------------------------------------------------

-- | Read environment variables and construct a 'ClonadEnv'.
-- Checks @OLLAMA_HOST@ first (for local Ollama), then @OPENAI_API_KEY@, then @ANTHROPIC_API_KEY@.
defaultEnv :: IO ClonadEnv
defaultEnv = do
  mOllama <- lookupEnv "OLLAMA_HOST"
  mModel <- lookupEnv "CLONAD_MODEL"
  mOpenAIBase <- lookupEnv "OPENAI_BASE_URL"
  case mOllama of
    Just url -> pure $ mkOllamaEnv (T.pack url) (maybe "qwen2.5:0.5b" T.pack mModel)
    Nothing ->
      lookupEnv "OPENAI_API_KEY" >>= \case
        Just key -> pure $ mkOpenAIEnv (T.pack key) (maybe "gpt-4o-mini" T.pack mModel) (T.pack <$> mOpenAIBase)
        Nothing ->
          lookupEnv "ANTHROPIC_API_KEY" >>= \case
            Nothing -> error "Clonad: none of OLLAMA_HOST, OPENAI_API_KEY, or ANTHROPIC_API_KEY set"
            Just key -> pure $ mkEnv (T.pack key)

-- | Construct a 'ClonadEnv' for Anthropic.
mkEnv :: Text -> ClonadEnv
mkEnv key =
  ClonadEnv
    { envBackend = Anthropic,
      envApiKey = key,
      envModel = "claude-sonnet-4-20250514",
      envTemperature = Nothing,
      envMaxTokens = 1024,
      envSystemPrompt = Nothing
    }

-- | Construct a 'ClonadEnv' for Ollama.
mkOllamaEnv :: Text -> Text -> ClonadEnv
mkOllamaEnv baseUrl model =
  ClonadEnv
    { envBackend = Ollama baseUrl,
      envApiKey = "",
      envModel = model,
      envTemperature = Nothing,
      envMaxTokens = 1024,
      envSystemPrompt = Nothing
    }

-- | Construct a 'ClonadEnv' for OpenAI or an OpenAI-compatible endpoint.
mkOpenAIEnv :: Text -> Text -> Maybe Text -> ClonadEnv
mkOpenAIEnv key model mBaseUrl =
  ClonadEnv
    { envBackend = OpenAI mBaseUrl,
      envApiKey = key,
      envModel = model,
      envTemperature = Nothing,
      envMaxTokens = 1024,
      envSystemPrompt = Nothing
    }

-- ---------------------------------------------------------------------------
-- Runner
-- ---------------------------------------------------------------------------

-- | Run a 'Clonad' computation.
runClonad :: ClonadEnv -> Clonad a -> IO a
runClonad env = flip runReaderT env . unClonad

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
clonad :: forall a b. (ClonadParam a, ClonadReturn b) => Text -> a -> Clonad b
clonad spec = clonadWith spec []

-- | Like 'clonad' but takes no input. Useful for generation tasks.
--
-- @
-- poem :: Clonad Text
-- poem = clonad_ "write a haiku about Haskell"
-- @
clonad_ :: forall b. (ClonadReturn b) => Text -> Clonad b
clonad_ spec = clonadWith spec [] ()

-- | Like 'clonad' but with additional conversation context prepended.
clonadWith ::
  forall a b.
  (ClonadParam a, ClonadReturn b) =>
  Text ->
  [(Text, Text)] ->
  a ->
  Clonad b
clonadWith spec _context input = Clonad $ ReaderT \env -> do
  let rspec = returnSpec (Proxy @b)
      serialised = serialise input
      systemMsg =
        T.unlines $
          catMaybes
            [ envSystemPrompt env,
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
  rawText <- callLLM env systemMsg userMsg
  case deserialise @b rawText of
    Left err ->
      error $
        "Clonad: failed to parse Claude response.\n"
          <> "  Spec:     "
          <> T.unpack spec
          <> "\n"
          <> "  Raw:      "
          <> T.unpack rawText
          <> "\n"
          <> "  Error:    "
          <> err
    Right val -> pure val

-- ---------------------------------------------------------------------------
-- HTTP Layer
-- ---------------------------------------------------------------------------

-- Anthropic response types
newtype ApiResponse = ApiResponse [ContentBlock]

data ContentBlock = ContentBlock !Text !Text -- type, text

instance FromJSON ApiResponse where
  parseJSON = Aeson.withObject "ApiResponse" \o -> ApiResponse <$> o .: "content"

instance FromJSON ContentBlock where
  parseJSON = Aeson.withObject "ContentBlock" \o ->
    ContentBlock <$> o .: "type" <*> o .: "text"

-- OpenAI/Ollama response types
newtype OllamaResponse = OllamaResponse [OllamaChoice]

newtype OllamaChoice = OllamaChoice OllamaMessage

newtype OllamaMessage = OllamaMessage Text

instance FromJSON OllamaResponse where
  parseJSON = Aeson.withObject "OllamaResponse" \o -> OllamaResponse <$> o .: "choices"

instance FromJSON OllamaChoice where
  parseJSON = Aeson.withObject "OllamaChoice" \o -> OllamaChoice <$> o .: "message"

instance FromJSON OllamaMessage where
  parseJSON = Aeson.withObject "OllamaMessage" \o -> OllamaMessage <$> o .: "content"

callLLM :: ClonadEnv -> Text -> Text -> IO Text
callLLM env systemMsg userMsg = case envBackend env of
  Anthropic -> callAnthropic env systemMsg userMsg
  Ollama base -> callOllama env base systemMsg userMsg
  OpenAI mBase -> callOpenAI env mBase systemMsg userMsg

callAnthropic :: ClonadEnv -> Text -> Text -> IO Text
callAnthropic env systemMsg userMsg = runReq defaultHttpConfig do
  let url = https "api.anthropic.com" /: "v1" /: "messages"
      headers =
        header "x-api-key" (TE.encodeUtf8 $ envApiKey env)
          <> header "anthropic-version" "2023-06-01"
          <> header "content-type" "application/json"
      body =
        object $
          [ "model" .= envModel env,
            "max_tokens" .= envMaxTokens env,
            "system" .= systemMsg,
            "messages" .= [object ["role" .= ("user" :: Text), "content" .= userMsg]]
          ]
            <> ["temperature" .= t | Just t <- [envTemperature env]]
  resp <- req POST url (ReqBodyJson body) jsonResponse headers
  let ApiResponse blocks = responseBody resp
  pure $ T.intercalate "\n" [txt | ContentBlock typ txt <- blocks, typ == "text"]

callOllama :: ClonadEnv -> Text -> Text -> Text -> IO Text
callOllama env baseUrl systemMsg userMsg = runReq defaultHttpConfig do
  let stripped = T.dropWhileEnd (== '/') baseUrl
      (scheme, rest) = T.breakOn "://" stripped
      hostPort = T.drop 3 rest
      (host, portPart) = T.breakOn ":" hostPort
      portNum = fromMaybe 11434 $ readMaybe (T.unpack $ T.drop 1 portPart)
      messages =
        [ object ["role" .= ("system" :: Text), "content" .= systemMsg],
          object ["role" .= ("user" :: Text), "content" .= userMsg]
        ]
      body =
        object $
          ["model" .= envModel env, "messages" .= messages]
            <> ["temperature" .= t | Just t <- [envTemperature env]]
      headers = header "content-type" "application/json"
  resp <-
    if scheme == "https"
      then req POST (https host /: "v1" /: "chat" /: "completions") (ReqBodyJson body) jsonResponse headers
      else req POST (http host /: "v1" /: "chat" /: "completions") (ReqBodyJson body) jsonResponse (headers <> port portNum)
  let OllamaResponse choices = responseBody resp
  pure $ case choices of
    (OllamaChoice (OllamaMessage content) : _) -> content
    [] -> ""

callOpenAI :: ClonadEnv -> Maybe Text -> Text -> Text -> IO Text
callOpenAI env mBaseUrl systemMsg userMsg = runReq defaultHttpConfig do
  let messages =
        [ object ["role" .= ("system" :: Text), "content" .= systemMsg],
          object ["role" .= ("user" :: Text), "content" .= userMsg]
        ]
      body =
        object $
          ["model" .= envModel env, "messages" .= messages]
            <> ["temperature" .= t | Just t <- [envTemperature env]]
      authHeader = header "Authorization" ("Bearer " <> TE.encodeUtf8 (envApiKey env))
      headers = authHeader <> header "content-type" "application/json"
  resp <- case mBaseUrl of
    Nothing ->
      -- Default OpenAI endpoint
      req POST (https "api.openai.com" /: "v1" /: "chat" /: "completions") (ReqBodyJson body) jsonResponse headers
    Just baseUrl -> do
      -- Custom OpenAI-compatible endpoint
      let stripped = T.dropWhileEnd (== '/') baseUrl
          (scheme, rest) = T.breakOn "://" stripped
          hostPort = T.drop 3 rest
          (host, portPart) = T.breakOn ":" hostPort
          portNum = fromMaybe 443 $ readMaybe (T.unpack $ T.drop 1 portPart)
      if scheme == "https"
        then req POST (https host /: "v1" /: "chat" /: "completions") (ReqBodyJson body) jsonResponse headers
        else req POST (http host /: "v1" /: "chat" /: "completions") (ReqBodyJson body) jsonResponse (headers <> port portNum)
  let OllamaResponse choices = responseBody resp
  pure $ case choices of
    (OllamaChoice (OllamaMessage content) : _) -> content
    [] -> ""

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

stripCodeFences :: Text -> Text
stripCodeFences t = case T.lines (T.strip t) of
  (first : rest)
    | "```" `T.isPrefixOf` first -> T.strip . T.unlines $ dropLast rest
  other -> T.unlines other
  where
    dropLast [] = []
    dropLast xs = init xs

-- ---------------------------------------------------------------------------
-- Combinators
-- ---------------------------------------------------------------------------

-- | Locally override the model.
withModel :: Text -> Clonad a -> Clonad a
withModel m = local \e -> e {envModel = m}

-- | Locally override the temperature.
withTemperature :: Double -> Clonad a -> Clonad a
withTemperature t = local \e -> e {envTemperature = Just t}

-- | Locally set a system prompt.
withSystemPrompt :: Text -> Clonad a -> Clonad a
withSystemPrompt s = local \e -> e {envSystemPrompt = Just s}
