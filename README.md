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

> Concurrency is unpredictable, can I get determinisim?

Of course, you can test `phyg` in `+Single-Threaded` mode!

```bash
cabal test integration-tests \
  --test-show-details=streaming \
  --project-file=single-threaded.project \
```

## Get newest version of PhyG (and other code)

To ensure that the integration test suite is invoking the *newest* version of `phyg`, it is important to clear the repository's local cache.
The local cache keeps a copy of external dependancies, specifically, the PHANE Project and PhyG code-bases.
Once there exists a local copy of these code-bases in the local cache, `cabal` will never look for newer versions of the PHANE Project or PhyG code.
Hence, clearing the cache is a requirement, as when the chace lacks a copy of the code-bases, this forces `cabal` to seek out and aquire the newest copy of the code-bases from GitHub to satisfy it's missing dependencies.

> I want to *get the newest code* and/or clear the local cache.

```bash
git pull     # Get the newest vesion of the test-suite
cabal clean  # Clear out potentially old PHANE & PhyG cached code
cabal update # Get new code from Hackage package database
cabal build tests --dry-run # Get new PHANE & PhyG code from GitHub
```

> I want to just build a copy of `phyg` to manually call.

```
cabal build    PhyG:exe:phyg # Build the binary
cabal list-bin PhyG:exe:phyg # Get the path of the binary
```

This is *multi-threaded* by default!
For single-threaed specify the alternative configuration file:

```
cabal build    PhyG:exe:phyg --project-file=single-threaded.project
cabal list-bin PhyG:exe:phyg --project-file=single-threaded.project
```


[GitHub-PhyG]: https://github.com/AMNH/PhyG#readme
