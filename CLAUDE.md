# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Clonad is a Claude Foreign Function Interface for Haskell. Instead of implementing functions, you describe their behaviour in natural language. The type signature serves as the contract, and Claude computes the result at runtime.

```haskell
-- FFI to C:
foreign import ccall "sin" c_sin :: Double -> Double

-- FFI to Claude:
sentiment :: Text -> Clonad Double
sentiment = clonad "return a sentiment score from -1.0 to 1.0"
```

## Build Commands

```sh
# Enter development shell (provides cabal, HLS, hlint, ormolu, ghcid)
nix develop

# Build
cabal build

# Run the server (requires ANTHROPIC_API_KEY or OLLAMA_HOST env var)
cabal run clonad-server

# Format code
nix fmt
```

## Environment Variables

- `ANTHROPIC_API_KEY` - Required for Claude backend
- `OLLAMA_HOST` - Use local Ollama instead (e.g., `http://localhost:11434`)
- `CLONAD_MODEL` - Override default model (defaults to `claude-sonnet-4-20250514` for Anthropic, `qwen2.5:0.5b` for Ollama)

## Architecture

### Core Module (`src/Clonad.hs`)

The entire library is in a single module exposing:

- **`Clonad` monad**: `ReaderT ClonadEnv IO` - carries API configuration through computations
- **`clonad` function**: The FFI primitive that takes a natural-language spec and returns a function `ClonadParam a => a -> Clonad b`
- **Typeclasses**:
  - `ClonadParam`: Types serialisable as input to Claude (Text, String, Int, Double, Bool, lists, tuples)
  - `ClonadReturn`: Types parseable from Claude's response with format specifications
- **Backends**: `Anthropic` (api.anthropic.com) or `Ollama` (local)
- **Combinators**: `withModel`, `withTemperature`, `withSystemPrompt` for local environment overrides

### Key Design Patterns

- Type signatures constrain Claude's output format via `returnSpec`
- Each `clonad` call makes a real HTTP request
- System prompt instructs Claude to behave as a "pure computation engine" returning only the computed result
- Response parsing uses Read for primitives, Aeson for JSON structures
