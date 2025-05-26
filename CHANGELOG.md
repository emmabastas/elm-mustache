# Changelog

This is a changelog for the Elm package, not the NPM package.
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

### Changed

### Removed

### Fixed

Exposed the `Context` alias (it was in public type signatures but not exposed)

### Security

## [1.0.1] - 2024-08-05

### Added

Support for parsing and interpolating
- Variables
- Sections
- Inverted sections
- Set delimiters

### Other notes

Known bug: `\r\n`-style line endings are converted to `\n` when they should be preserved.


[1.0.0]: https://github.com/emmabastas/elm-mustache/releases/tag/1.0.0
