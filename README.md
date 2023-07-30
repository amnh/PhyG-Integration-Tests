PhyG Integration Test Suite
===========================

A composition of integration tests for the [**`PhyG`**][GitHub-PhyG] software program.


## How to run

```bash
cabal test integration-tests \
  --test-show-details=streaming
```

> But what if I don't want to run *everything?*

Oh, that's easy... try this:

```bash
cabal test integration-tests \
  --test-show-details=streaming \
  --test-options="--pattern /t*/"
```

Be sure to put `--pattern` before any other test options (such as `--speedy-subset`) or else the finiky test-suite will freak out.

> Can I just run the "fast" test cases?

Of course, your time is valuable! Ask for the `--speedy-subset`.

```bash
cabal test integration-tests \
  --test-show-details=streaming \
  --test-options="--speedy-subset"
```

[GitHub-PhyG]: https://github.com/AMNH/PhyG#readme
