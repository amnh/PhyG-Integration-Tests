PhyG Integration Tests
=======================

# Changelog

PHANE uses [Semantic Versioning (v2.0.0)][SemVer-URI].
The changelog is available [on GitHub][GitHub-Changelog].


## Unreleased (`v0.2.0`)

  * Added `--speedy-subset` CLI option to skip the long running integration test cases

  * Added `data/speedy.txt` as an *inclusive* set of test cases which complete execution "quickly"

  * Corrected `Single-Threaded` flag to have correct semantics

  * Enhanced `tests/remake-golden-cabal-run.sh` to be more robust

  * Enhanced `.gitignore` to reduce noise when using `git status`

  * Updated to correctly locate the newest `phyg` executable to invoke test cases

  * Updated to always display ***colored*** test case results

  * Moved from PhyG repository


## `v0.1.0`

  * Initial "alpha" state of integration test suite


[GitHub-Changelog]: https://github.com/AMNH/PhyG-Integration-Tests/Changelog.md
[SemVer-URI]: https://semver.org/spec/v2.0.0.html
