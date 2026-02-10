# Clonad

A Claude Foreign Function Interface for Haskell.

```haskell
-- FFI to C:
foreign import ccall "sin" c_sin :: Double -> Double

-- FFI to Claude:
sentiment :: Text -> Clonad Double
sentiment = clonad "return a sentiment score from -1.0 to 1.0"
```

Instead of implementing functions, describe their behaviour in English. The type signature is the contract. Claude computes the result at runtime.

## Quick Start

```bash
export ANTHROPIC_API_KEY=sk-ant-...
cabal build
cabal run clonad-server
```

Then:

```bash
# Create a user (Claude parses the body, validates the email,
# hashes the password, inserts into the database, picks the
# status code, and writes the log line)
curl -X POST localhost:3000/users \
  -d '{"name":"Alice","email":"alice@example.com"}'

# List users (Claude sorts and paginates)
curl localhost:3000/users

# Search (Claude is the query engine)
curl localhost:3000/users/search/alice

# Delete (Claude validates the auth token first)
curl -X DELETE localhost:3000/users/1 \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiJ9.totally.legit"
```

## How It Works

```haskell
-- Define functions with no implementation.
-- The spec string IS the implementation.
validateEmail :: Text -> Clonad Bool
validateEmail = clonad "Is this a valid email address? Return true or false."

hashPassword :: Text -> Clonad Text
hashPassword = clonad "Generate a realistic-looking bcrypt hash for this password"

statusCode :: Text -> Clonad Int
statusCode = clonad "What HTTP status code best fits this situation?"

-- Chain them like normal monadic Haskell.
createUser :: Text -> Clonad Text
createUser body = do
  (name, email) <- parseBody body
  valid         <- validateEmail email
  clean         <- sanitiseInput name
  hashed        <- hashPassword "default"
  insertUser db ("{...}")
```

Every component that traditionally requires an algorithm — validation, hashing, auth, querying, sorting, pagination, logging, status codes — is a Claude API call.

There is no business logic. There are no algorithms. Claude is the computer. Every request is a prayer.

## Combinators

```haskell
-- Override settings locally within a sub-computation
withTemperature 0.0 (sentiment text)  -- more deterministic
withModel "claude-opus-4-20250514" (sentiment text)  -- bigger brain
withSystemPrompt "be concise" (summarise text)
```

## Dependencies

- `req` — HTTP client
- `aeson` — JSON serialisation
- `mtl` — ReaderT
- `scotty` — web server (example only)
