PhyG Integration Tests
=======================

# Changelog

PHANE uses [Semantic Versioning (v2.0.0)][SemVer-URI].
The changelog is available [on GitHub][GitHub-Changelog].


## Unreleased (`v0.2.0`)

  * Added `--subset-hours` CLI option to skip the long running integration test cases

  * Added `--subset-rapid` CLI option to skip all but the fastest integration test cases

  * Added `data/subset-hours.txt` as an *inclusive* set of test cases which completes within a few hours

  * Added `data/subset-rapid.txt` as an *inclusive* set of test cases which completes "rapidly"

  * Corrected `Single-Threaded` flag to have correct semantics

  * Enhanced `tests/remake-golden-cabal-run.sh` to be more robust

  * Enhanced `.gitignore` to reduce noise when using `git status`

  * Updated to correctly locate the newest `phyg` executable to invoke test cases

  * Updated to always display ***colored*** test case results

  * Updated `data/subset-hours.txt` with an initial set of test cases with *"moderate"* runtime

  * Updated `data/subset-rapid.txt` with an initial set of test cases with *"rapid"* runtime

  * Moved from PhyG repository


## `v0.1.0`

  * Initial "alpha" state of integration test suite


[GitHub-Changelog]: https://github.com/AMNH/PhyG-Integration-Tests/Changelog.md
[SemVer-URI]: https://semver.org/spec/v2.0.0.html
