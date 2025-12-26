# Change Log
All notable changes to this project will be documented in this file. This change
log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

Semantic versioning isn't strictly followed: at least so long as we're 0.x.x
minor versions are incremented whenever, and as much, it feels warranted by
the new features.

## [Unreleased]

## [0.0.4] - 2025-12-25

### Added

- Column data diagnosis for detecting simple types (tags, numbers, some dates).
- Improved detection of categorical-numerical data.
- Preprocessing function priorities.
- Consistent columns for multihot encoding raw columns, skip nil values.
- Extended testing for Anime Database along with fixes and polishing some logic.

## [0.0.3] - 2025-10-16

### Added
- Data diagnostics recognize numbers and some datetime formats
- Integrated mill functions for recommending options similar to the known interactions
  and options from similar cases (if no interactions)
- Centralized preprocessing for columns, transformations prepared once, and items
  from getters are wrapped in them

### Changed
- Moved the math to a separate namespace so Neanderthal can be swapped for
  something else e.g. for targetting WebAssembly with GraalVM.
- Mills now handle getting more data and retrying interaction in a unified way
  (like finite state automatons)

## [0.0.1] - 2025-07-18

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
[0.0.4]: 1bff836
[0.0.3]: d557326
[0.0.1]: d7b0fa1
[0.0.1-init]: (initial commit) 27a8787
