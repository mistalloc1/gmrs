# Change Log
All notable changes to this project will be documented in this file. This change
log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

Semantic versioning isn't strictly followed: at least so long as we're 0.x.x
minor versions are incremented whenever, and as much, it feels warranted by
the new features.

## [Unreleased]

### Added
- Data diagnostics recognize numbers and some datetime formats
- A separate mill function for recommending options similar to the known interactions

### Changed
- Moved the math to a separate namespace so Neanderthal can be swapped for
  something else e.g. for targetting WebAssembly with GraalVM.

## [0.0.1]

### Added
- CSV gives and API function for sending the contents.
- Pull strategies and their use for calling recommend mill functions.

### Changed
- Gives as lazy sequences.
- Reorganized the getters used by command and governor namespaces, decoupled
  from the global state.

### Fixed
- Option, case gives/sends (baseless version) now work.

## [0.0.1-init] - 2025-07-09

[Unreleased]: ...
[0.0.1]: d7b0fa1
[0.0.1-init]: (initial commit) 27a8787
