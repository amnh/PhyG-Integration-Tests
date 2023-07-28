PhyG Integration Test Suite
===========================

A composition of integration tests for the [**`PhyG`**][GitHub-PhyG] software program.


## How to run

```bash
cabal test integration-tests \
  --test-show-details=streaming \
  --test-options="--color=always"
```

> But what if I don't want to run *everything?*

Oh, that's easy... try this:

```bash
cabal test integration-tests \
  --test-show-details=streaming \
  --test-options="--pattern /t*/ --color=always"
```

Be sure to put `--pattern` before `--color` or else the finiky test-suite will freak out.


[GitHub-PhyG]: https://github.com/AMNH/PhyG#readme
