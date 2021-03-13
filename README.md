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


## Overview of the dangerous functions

TODO: This section isn't finished yet.

### Memory-unsafe functions
#### `unsafeDupablePerformIO`

TODO: Unsafe

#### `unsafeInterleaveIO`

TODO: Unsafe

#### `unsafeFixIO`

TODO: Unsafe

#### `unsafePerformIO`

TODO: Unsafe

### Functions with issues related to threading

#### `forkIO`

_VERY_ hard to get right, use the async library instead.
See also https://github.com/informatikr/hedis/issues/165

#### `forkProcess`

Mostly impossible to get right, rethink what you're doing entirely.
See also https://www.reddit.com/r/haskell/comments/jsap9r/how_dangerous_is_forkprocess/

### Functions that purposely throw exceptions in pure code

#### `undefined`

Purposely fails. Deal with errors appropriately instead.

#### `throw`

Don't throw from pure code, use throwIO instead.

#### `error`

Purposely fails. Deal with errors appropriately instead.


### Partial functions

#### `head`
#### `tail`
#### `init`
#### `last`
#### `'!!'`
#### `toEnum`

#### `quot`
see also https://github.com/NorfairKing/haskell-WAT#num-int
#### `div`
#### `rem`
#### `mod`
#### `quotRem`
#### `divMod`

#### `read`

Partial, use  use `Text.Read.readMaybe` instead.

#### `fromJust`

Partial


### Confusing functions

#### `until`

Really confusing, use 'when' instead.

### Functions with unexpected performance characteristics

#### `nub`

O(n^2), use [`ordNub`](https://github.com/nh2/haskell-ordnub) instead

#### `concat`

O(n^2)

#### `foldl`

Lazy. Use foldl' instead.

#### `sum` and `product`
Lazy accumulator, but is fixed as of [GHC 9.0.1](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4675).


### Functions that do unexpected things

#### `realToFrac`

See https://github.com/NorfairKing/haskell-WAT#real-double


#### `fromIntegral`

Does silent truncation:
```
> fromIntegral (300 :: Word) :: Word8
44
```

### Deprecated

Use `pure` instead.
See https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-of-no-return


### Functions that don't do what you think they do

#### `System.IO.putChar`
#### `System.IO.putStr`
#### `System.IO.putStrLn`
#### `System.IO.print`

#### `System.IO.getChar`
#### `System.IO.getLine`
#### `System.IO.getContents`
#### `System.IO.interact`
#### `System.IO.readIO`
#### `System.IO.readLn`

#### `System.IO.readFile`
#### `System.IO.writeFile`
#### `System.IO.appendFile`

#### `Data.Text.IO.readFile`
#### `Data.Text.IO.Lazy.readFile`





