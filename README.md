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

Be sure to put `--pattern` before any other test options (such as `--subset-rapid`) or else the finiky test-suite will freak out.

> This takes forever, can I get feedback within a minute or two? 

Of course, your time is valuable! Ask for the `--subset-rapid`.

```bash
cabal test integration-tests \
  --test-show-details=streaming \
  --test-options="--subset-rapid"
```

> What if I want this to take hours not days?

You want a thorough, but not complete, sampling... request `--subset-hours`.

```bash
cabal test integration-tests \
  --test-show-details=streaming \
  --test-options="--subset-hours"
```

[GitHub-PhyG]: https://github.com/AMNH/PhyG#readme
