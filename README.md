PhyG Integration Test Suite
===========================

A composition of integration tests for the [**`PhyG`**][GitHub-PhyG] software program.


## How to run

```bash
cabal test integration-tests \
  --test-show-details=streaming \
  --test-option="--color=always"
```

[GitHub-PhyG]: https://github.com/AMNH/PhyG#readme
