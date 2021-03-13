# Haskell's Dangerous Functions

## What does dangerous mean?

Dangerous could mean either of these:

- Partial: can throw exceptions in pure code
- Unsafe: can cause segfaults
- Has unexpected performance characteristics
- Doesn't do what you want
- Doesn't do what you think it does

## How to forbid these dangerous functions in your codebase

1. Copy the `hlint.yaml` file in this repository to `.hlint.yaml` within your repository

    ```
    cat /path/to/haskell-dangerous-functions >> /path/to/your/project/.hlint.yaml
    ```

2. Run `hlint` on your code.
   Make sure to require new changes to be `hlint`-clean.
   You can use `hlint --default` to generate a settings file ignoring all the hints currently outstanding.
   You can use [pre-commit hooks](https://pre-commit.com/) to forbid committing non-`hlint`-clean changes.


## FAQ


* **It seems pretty silly that these functions still exist, and their dangers not well-documented.**

  I know! See [the relevant discussion on the GHC issue tracker](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5206).

* **Surely everyone knows about these?**

  Maybe, but I certainly didn't, until they caused real production issues.

