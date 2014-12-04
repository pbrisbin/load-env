# load-env

This is effectively a port of [dotenv][], whose README explains it best:

> Storing configuration in the environment is one of the tenets of a
> twelve-factor app. Anything that is likely to change between deployment
> environments–such as resource handles for databases or credentials for
> external services–should be extracted from the code into environment
> variables.
>
> But it is not always practical to set environment variables on development
> machines or continuous integration servers where multiple projects are run.
> dotenv loads variables from a .env file into ENV when the environment is
> bootstrapped.

[dotenv]: https://github.com/bkeepers/dotenv

This library exposes functions for doing just that.

## Installation

```
% cabal update
% cabal install load-env
```

## Usage

```haskell
import LoadEnv
import System.Environment (lookupEnv)

main :: IO ()
main = do
    loadEnv

    print =<< lookupEnv "FOO"

-- % cat .env
-- FOO=bar
-- % runhaskell main.hs
-- Just "bar"
```

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```
