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

### [`forkIO`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Concurrent.html#v:forkIO)

TL;DR: Using `forkIO` is _VERY_ hard to get right, use the async library instead.

The main issue is that when threads spawned using `forkIO` throw an exception, this exception is not rethrown in the thread that spawned that thread.

As an example, suppose we `forkIO` a server and something goes wrong.
The main thread will not notice that anything went wrong.
The only indication that an exception was thrown will be that something is printed on `stderr`. 

``` haskell
$ cat test.hs
#!/usr/bin/env stack
-- stack --resolver lts-15.15 script
{-# LANGUAGE NumericUnderscores #-}
import Control.Concurrent
main :: IO ()
main = do
  putStrLn "Starting our 'server'."
  forkIO $ do            
    putStrLn "Serving..."
    threadDelay 1_000_000
    putStrLn "Oh no, about to crash!"
    threadDelay 1_000_000
    putStrLn "Aaaargh"
    undefined
  threadDelay 5_000_000
  putStrLn "Still running, eventhough we crashed"
  threadDelay 5_000_000                 
  putStrLn "Ok that's enough of that, stopping here."
```

Which outputs:

```
$ ./test.hs
Starting our 'server'.
Serving...
Oh no, about to crash!
Aaaargh
test.hs: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
  undefined, called at /home/syd/test/test.hs:17:5 in main:Main
Still running, eventhough we crashed
Ok that's enough of that, stopping here.
```

Instead, we can use [`concurrently_` from the `async` package](http://hackage.haskell.org/package/async-2.2.3/docs/Control-Concurrent-Async.html#v:concurrently_):

``` haskell
$ cat test.hs
-- stack --resolver lts-15.15 script

{-# LANGUAGE NumericUnderscores #-}

import Control.Concurrent
import Control.Concurrent.Async

main :: IO ()
main = do
  putStrLn "Starting our 'server'."
  let runServer = do
        putStrLn "Serving..."
        threadDelay 1_000_000
        putStrLn "Oh no, about to crash!"
        threadDelay 1_000_000
        putStrLn "Aaaargh"
        undefined
  let mainThread = do
        threadDelay 5_000_000
        putStrLn "Still running, eventhough we crashed"
        threadDelay 5_000_000
        putStrLn "Ok that's enough of that, stopping here."
  concurrently_ runServer mainThread
```

to output:

```
$ ./test.hs
Starting our 'server'.
Serving...
Oh no, about to crash!
Aaaargh
test.hs: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
  undefined, called at /home/syd/test.hs:18:9 in main:Main
```

See also:

* https://github.com/informatikr/hedis/issues/165
* https://github.com/hreinhardt/amqp/issues/96

### Partial functions

#### `head`

Throws an exception in pure code when the input is an empty list.

```
Prelude> head []
*** Exception: Prelude.head: empty list
```

#### `tail`

Throws an exception in pure code when the input is an empty list.

```
Prelude> tail []
*** Exception: Prelude.tail: empty list
```

#### `init`

Throws an exception in pure code when the input is an empty list.

```
Prelude> init []
*** Exception: Prelude.init: empty list
```

#### `last`

Throws an exception in pure code when the input is an empty list.

```
Prelude> last []
*** Exception: Prelude.last: empty list
```

#### `'!!'`

Throws an exception in pure code when the index is out of bounds.

```
Prelude> [1, 2, 3] !! 3
*** Exception: Prelude.!!: index too large
```

It also allows negative indices, for which it also throws.

```
Prelude> [1,2,3] !! (-1)
*** Exception: Prelude.!!: negative index
```


## Dangerous functions about which no explanation has been written yet

TODO: This section isn't finished yet.

### Memory-unsafe functions
#### `unsafeDupablePerformIO`

TODO: Unsafe",

#### `unsafeInterleaveIO`

TODO: Unsafe

#### `unsafeFixIO`

TODO: Unsafe

#### `unsafePerformIO`

TODO: Unsafe

### Functions with issues related to threading


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





