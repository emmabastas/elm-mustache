# Changelog

This is a changelog for the Elm package, not the NPM package.
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

These are the types of changes:
 - **Added** for new features.
 - **Changed** for changes in existing functionality.
 - **Removed** for now removed features.
 - **Fixed** for any bug fixes.
 - **Security** in case of vulnerabilities.

## UNRELEASED [1.2.1]

### Fixed

- Typos in documentation (ab8e2bf)

## [1.2.0] - 2025-06-20

### Added

- New type `Tag` with function `tags : Ast -> List Tags`. This lets a user get certain information from a parsed mustache template, like the names of all tags. This was implemented in response to https://github.com/emmabastas/elm-mustache/issues/1

### Fixed

Exposed the `Name` alias (it was in public type signatures but not exposed)

## [1.1.1] - 2025-05-27

### Added

- Partial tags.

## [1.1.0] - 2025-05-26

### Fixed

Exposed the `Context` alias (it was in public type signatures but not exposed)

## [1.0.0] - 2024-08-05

### Added

Support for parsing and interpolating
- Variables
- Sections
- Inverted sections
- Set delimiters

### Other notes

Known bug: `\r\n`-style line endings are converted to `\n` when they should be preserved.


[1.2.1]: https://github.com/emmabastas/elm-mustache/releases/tag/1.2.1
[1.2.0]: https://github.com/emmabastas/elm-mustache/releases/tag/1.2.0
[1.1.1]: https://github.com/emmabastas/elm-mustache/releases/tag/1.1.1
[1.1.0]: https://github.com/emmabastas/elm-mustache/releases/tag/1.1.0
[1.0.0]: https://github.com/emmabastas/elm-mustache/releases/tag/1.0.0
