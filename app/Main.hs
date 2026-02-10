{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Example web server powered entirely by the Clonad FFI
--
-- A CRUD web server in which every component that would traditionally
-- require an implementation (input validation, password hashing,
-- authentication, database operations, sorting, pagination, logging,
-- HTTP status code selection) is instead a Claude API call.
--
-- There is no business logic. There are no algorithms. Claude is the
-- computer.

module Main where

import Clonad

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Network.HTTP.Types.Status (status404)
import Network.Wai qualified as Wai
import Web.Scotty

-- ---------------------------------------------------------------------------
-- The "Database"
-- ---------------------------------------------------------------------------

type DB = IORef Text

-- ---------------------------------------------------------------------------
-- Claude FFI Declarations
--
-- Each of these is a function signature with no implementation.
-- The specification string is the implementation.
-- ---------------------------------------------------------------------------

-- ORM -----------------------------------------------------------------------

insertUser :: Text -> Text -> Clonad Text
insertUser db =
  clonad $
    "You are a JSON database engine. Here is the current database:\n"
      <> db
      <> "\nInsert this new user record into the array. Assign an auto-incrementing \
         \integer \"id\" field. Preserve all existing records exactly. Return the \
         \ENTIRE updated JSON array and nothing else."

queryUsers :: Text -> Text -> Clonad Text
queryUsers db =
  clonad $
    "You are a JSON database engine. Here is the current database:\n"
      <> db
      <> "\nRun this natural-language query against the data. Return ONLY the \
         \matching records as a JSON array."

deleteUser :: Text -> Text -> Clonad Text
deleteUser db =
  clonad $
    "You are a JSON database engine. Here is the current database:\n"
      <> db
      <> "\nDelete the record matching this criteria. Return the ENTIRE updated \
         \JSON array and nothing else."

-- Auth ----------------------------------------------------------------------

hashPassword :: Text -> Clonad Text
hashPassword =
  clonad
    "Generate a realistic-looking bcrypt hash string for this password. \
    \It does not need to be cryptographically valid, just structurally \
    \correct (starting with $2b$ and the right length). Return ONLY the hash."

validateToken :: Text -> Clonad Bool
validateToken =
  clonad
    "Decide whether this authorization token looks valid and non-expired. \
    \Consider length, format, and structure. Return true or false."

-- Validation ----------------------------------------------------------------

validateEmail :: Text -> Clonad Bool
validateEmail = clonad "Is this a valid email address? Return true or false."

sanitiseInput :: Text -> Clonad Text
sanitiseInput =
  clonad
    "Sanitise this user input for safe HTML display. Escape any XSS vectors, \
    \strip dangerous tags or attributes. Return the cleaned string."

-- HTTP Semantics ------------------------------------------------------------

statusCode :: Text -> Clonad Int
statusCode =
  clonad
    "What HTTP status code best fits this situation description? Return ONLY \
    \the integer."

-- Request Parsing -----------------------------------------------------------

parseBody :: Text -> Clonad (Text, Text)
parseBody =
  clonad
    "Extract the \"name\" and \"email\" fields from this JSON body. Return a \
    \JSON array of exactly two strings: [name, email]."

-- Data Manipulation ---------------------------------------------------------

sortUsers :: Text -> Text -> Clonad Text
sortUsers db =
  clonad $
    "Here is a JSON array of user records:\n"
      <> db
      <> "\nSort all records by this field in ascending order. Return the sorted \
         \JSON array and nothing else."

paginate :: Text -> Text -> Clonad Text
paginate db =
  clonad $
    "Here is a JSON array:\n"
      <> db
      <> "\nThis is a query string from an HTTP request. Parse \"page\" and \
         \\"per_page\" parameters (defaulting to page=1, per_page=10). Return \
         \ONLY the appropriate slice of records as a JSON array."

-- Observability -------------------------------------------------------------

formatLog :: Text -> Clonad Text
formatLog =
  clonad
    "Format this as a structured log line. Include a realistic ISO8601 \
    \timestamp (invent one), a log level, and the message. Return ONLY the \
    \log line, e.g.: 2025-02-09T14:23:01Z [INFO] created user: Alice"

-- ---------------------------------------------------------------------------
-- Server
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  env <- defaultEnv
  db <- newIORef ("[]" :: Text)
  putStrLn "Clonad server starting on port 3000..."
  putStrLn "Every request is a prayer."

  scotty 3000 do
    -- CREATE ----------------------------------------------------------------
    post "/users" do
      rawText <- TL.toStrict . TLE.decodeUtf8 <$> body

      (code, response) <- liftIO $ runClonad env do
        (name, email) <- parseBody rawText
        validEmail <- validateEmail email
        cleanName <- sanitiseInput name
        if not validEmail
          then (,"invalid email") <$> statusCode "client provided an invalid email address"
          else do
            hashed <- hashPassword "default_password"
            current <- liftIO $ readIORef db
            let record =
                  "{\"name\":\""
                    <> cleanName
                    <> "\",\"email\":\""
                    <> email
                    <> "\",\"password_hash\":\""
                    <> hashed
                    <> "\"}"
            newDb <- insertUser current record
            liftIO $ writeIORef db newDb
            logLine <- formatLog ("created user: " <> cleanName)
            liftIO $ putStrLn (T.unpack logLine)
            (,newDb) <$> statusCode "resource successfully created"

      status (toEnum code)
      text (TL.fromStrict response)

    -- READ ------------------------------------------------------------------
    get "/users" do
      qs <- Wai.queryString <$> request
      current <- liftIO $ readIORef db

      (code, paginated) <- liftIO $ runClonad env do
        sorted <- sortUsers current "name"
        paginated <- paginate sorted (T.pack $ show qs)
        (,paginated) <$> statusCode "successful data retrieval"

      status (toEnum code)
      text (TL.fromStrict paginated)

    -- SEARCH ----------------------------------------------------------------
    get "/users/search/:query" do
      q <- captureParam "query"
      current <- liftIO $ readIORef db

      (code, matches) <- liftIO $ runClonad env do
        matches <- queryUsers current q
        (,matches) <$> statusCode "search completed successfully"

      status (toEnum code)
      text (TL.fromStrict matches)

    -- DELETE ----------------------------------------------------------------
    delete "/users/:uid" do
      uid <- captureParam "uid"
      tok <- maybe "" TL.toStrict <$> header "Authorization"
      current <- liftIO $ readIORef db

      (code, response) <- liftIO $ runClonad env do
        authed <- validateToken tok
        if not authed
          then (,"unauthorized") <$> statusCode "unauthorized request, missing or invalid token"
          else do
            newDb <- deleteUser current ("id = " <> uid)
            liftIO $ writeIORef db newDb
            logLine <- formatLog ("deleted user id: " <> uid)
            liftIO $ putStrLn (T.unpack logLine)
            (,newDb) <$> statusCode "resource successfully deleted"

      status (toEnum code)
      text (TL.fromStrict response)

    -- 404 -------------------------------------------------------------------
    notFound do
      result <-
        liftIO $
          runClonad env $
            (clonad :: Text -> Text -> Clonad Text)
              "Generate a short, professional JSON error response for a 404"
              "The requested resource was not found"
      status status404
      text (TL.fromStrict result)
