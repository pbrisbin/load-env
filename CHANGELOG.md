## [*Unreleased*](https://github.com/pbrisbin/load-env/compare/v0.2.1.0...main)

None

## [v0.2.1.0](https://github.com/pbrisbin/load-env/compare/v0.2.0.2...v0.2.1.0)

- Don't override values already set in the environment

  Given a hypothetical program `load-env`, which uses one of our `loadEnv`
  functions on `stdin`:

  Previously,

  ```
  FOO=bar load-env <<EOM
  FOO=bat
  EOM
  ```

  would override `FOO` to `bat` when `load-env` ran. But now, it will see `FOO`
  is already `bar` and leave it.

  This is better behavior under the assumption that a `.env` file is meant to
  specify *defaults* in the case of nothing explicit. When there are explicit
  values in the environment, it's most likely that our user indeed wants them
  respected.

## [v0.2.0.2](https://github.com/pbrisbin/load-env/compare/v0.2.0.1...v0.2.0.2)

- Allow lower-case characters in variable names [@denibertovic](https://github.com/pbrisbin/load-env/pull/4)

## [v0.2.0.1](https://github.com/pbrisbin/load-env/compare/v0.2.0.0...v0.2.0.1)

- Packaging and documentation updates

## [v0.2.0.0](https://github.com/pbrisbin/load-env/compare/v0.1.2...v0.2.0.0)

- Traverse up parent directories to find the `.env` file

## [v0.1.2](https://github.com/pbrisbin/load-env/compare/v0.1.1...v0.1.2)

- Packaging updates

## [v0.1.1](https://github.com/pbrisbin/load-env/compare/v0.1.0...v0.1.1)

- Parse variables names more strictly

## [v0.1.0](https://github.com/pbrisbin/load-env/compare/v0.0.4...v0.1.0)

- Don't fail on an empty file
- Ignore any invalid lines, not specifically things that look like comments

## [v0.0.4](https://github.com/pbrisbin/load-env/compare/v0.0.3...v0.0.4)

- Don't throw an exception if the `.env` file is missing

## [v0.0.3](https://github.com/pbrisbin/load-env/compare/v0.0.2...v0.0.3)

- Variable names can contain underscores

## [v0.0.2](https://github.com/pbrisbin/load-env/compare/v0.0.1...v0.0.2)

- Drop support for GHC < 7.8

## [v0.0.1](https://github.com/pbrisbin/load-env/tree/v0.0.1)

Initial release.
