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

3. Whenever you want to make an exception, and use a forbidden function anyway, use the `ignore` key to add an exception to the `.hlint.yaml` file.
  

## FAQ


* **It seems pretty silly that these functions still exist, and their dangers not well-documented.**

  I know! See [the relevant discussion on the GHC issue tracker](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5206).

* **Surely everyone knows about these?**

  Maybe, but I certainly didn't, until they caused real production issues.

## Contributing

**WANTED: Evidence of the danger in these functions.**
If you can showcase an public incident with real-world consequences that happened because of one of these functions, I would love to refer to it in this document!


If you know about another dangerous function that should be avoided, feel free to submit a PR!
Please include:

* an `hlint` config to forbid the function in [`hlint.yaml`](hlint.yaml).
* a section in this document with:
  * Why the function is dangerous
  * A reproducable way of showing that it is dangerous.
  * An alternative to the dangerous function

It might be that the function you have in mind is not dangerous but still weird.
In that case you can add it to [the Haskell WAT list](https://github.com/NorfairKing/haskell-WAT).



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

#### [`forkProcess`](https://hackage.haskell.org/package/unix-2.7.2.2/docs/System-Posix-Process.html#v:forkProcess)

Mostly impossible to get right.
You probably want to be using [the `async` library](http://hackage.haskell.org/package/async) instead.

If you think "I know what I'm doing" then you're probably still wrong.
Rethink what you're doing entirely.

See also https://www.reddit.com/r/haskell/comments/jsap9r/how_dangerous_is_forkprocess/

### Partial functions

#### [`head`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:head)

Throws an exception in pure code when the input is an empty list.

```
Prelude> head []
*** Exception: Prelude.head: empty list
```

Use `listToMaybe` instead.

#### [`tail`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:tail)

Throws an exception in pure code when the input is an empty list.

```
Prelude> tail []
*** Exception: Prelude.tail: empty list
```

Use a case-match instead.

#### [`init`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:init)

Throws an exception in pure code when the input is an empty list.

```
Prelude> init []
*** Exception: Prelude.init: empty list
```

Use a case-match on the `reverse` of the list instead, but keep in mind that it uses linear time in the length of the list.
Use a different data structure if that is an issue for you.


#### [`last`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:last)

Throws an exception in pure code when the input is an empty list.

```
Prelude> last []
*** Exception: Prelude.last: empty list
```

Use a `listToMaybe . reverse` instead, but keep in mind that it uses linear time in the length of the list.
Use a different data structure if that is an issue for you.

#### [`'!!'`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:-33--33-)

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

The right way index is to not use a list, because list indexing takes `O(n)` time, even if you find a safe way to do it.
If you _really_ need to deal with _list_ indexing (you don't), then you can use a combination of [`take`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:take) and [`drop`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:drop).

#### [`fromJust`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Maybe.html#v:fromJust)

Throws an exception _in pure code_ when the input is `Nothing`.

```
Prelude Data.Maybe> fromJust Nothing
*** Exception: Maybe.fromJust: Nothing
CallStack (from HasCallStack):
  error, called at libraries/base/Data/Maybe.hs:148:21 in base:Data.Maybe
  fromJust, called at <interactive>:11:1 in interactive:Ghci1
```

Use a case-match instead.

#### [`read`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-Read.html#v:read)

There are multiple reasons not to use `read`.
The most obvious one is that it is partial.
It throws an exception _in pure code_ whenever the input cannot be parsed (and doesn't even give a helpful parse error):

```
Prelude> read "a" :: Int
*** Exception: Prelude.read: no parse
```

You can use `readMaybe` to get around this issue, HOWEVER:

The second reason not to use `read` is that it operates on `String`.

``` haskell
read :: Read a => String -> a
```

If you are doing any parsing, you should be using a more appropriate data type to parse: (`Text` or `ByteString`)

The third reason, is that `read` comes from [the `Read` type class](https://hackage.haskell.org/package/base/docs/Text-Read.html#t:Read), which has no well-defined semantics.
In an ideal case, `read` and `show` would be inverses but this is _just not the reality_.
See [`UTCTime`](https://hackage.haskell.org/package/time/docs/Data-Time-Clock.html#t:UTCTime) as an example.

### Functions involving division

* [`quot`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:quot)
* [`div`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:div)
* [`rem`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:rem)
* [`mod`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:mod)
* [`divMod`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:divMod)
* [`quotRem`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:quotRem)

```
Prelude> quot 1 0
*** Exception: divide by zero
Prelude> minBound `quot` (-1) :: Int
*** Exception: arithmetic overflow
Prelude> div 1 0
*** Exception: divide by zero
Prelude> minBound `div` (-1) :: Int
*** Exception: arithmetic overflow
Prelude> rem 1 0
*** Exception: divide by zero
Prelude> mod 1 0
*** Exception: divide by zero
Prelude> divMod 1 0
*** Exception: divide by zero
Prelude> quotRem 1 0
*** Exception: divide by zero
```

Whenever you consider using division, _really_ ask yourself whether you need division.
For example, you can (almost always) replace ``a `div` 2 <= b`` by `a <= 2 * b`.
(If you're worried about overflow, then use a bigger type.)

If your use-case has a fixed (non-`0`) _literal_ denominator, like ``a `div` 2``, and you have already considered using something other than division, then your case constitutes an acceptable exception.

Note that integer division may not be what you want in the first place anyway:

```
Prelude> 5 `div` 2
2 -- Not 2.5
```

See also https://github.com/NorfairKing/haskell-WAT#num-int


### Functions that purposely throw exceptions in pure code on purpose

#### [`throw`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html#v:throw)

Purposely throws an exception _in pure code_.

```
Prelude Control.Exception> throw $ ErrorCall "here be a problem"
*** Exception: here be a problem
```

Don't throw from pure code, use throwIO instead.

#### [`undefined`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:undefined)

Purposely fails, with a particularly unhelpful error message.

```
Prelude> undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
  undefined, called at <interactive>:1:1 in interactive:Ghci1
```

Deal with errors appropriately instead.

Also see [`error`] below.

#### [`error`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:error)

Purposely fails, with an only slightly less unhelpful error message than `undefined`.

```
Prelude> error "here be a problem"
*** Exception: here be a problem
CallStack (from HasCallStack):
  error, called at <interactive>:4:1 in interactive:Ghci1
```

Deal with errors appropriately instead.

If you're _really very extra sure_ that a certain case will never happen.
Bubble up the error to the `IO` part of your code and then use `throwIO` or `die`.


### Functions that do unexpected things

#### [`realToFrac`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:realToFrac)

This function goes through `Rational`:

```
-- | general coercion to fractional types
realToFrac :: (Real a, Fractional b) => a -> b
realToFrac = fromRational . toRational
```

`Rational` does does not have all the values that a `Real` like `Double` might have, so things will go wrong in ways that you don't expect:

```
Prelude> realToFrac nan :: Double
-Infinity
```

Avoid general coercion functions and anything to do with `Double` in particular.

See also https://github.com/NorfairKing/haskell-WAT#real-double

#### [`fromIntegral`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:fromIntegral)

`fromIntegral` has no constraints on the size of the output type, so that output type could be smaller than the input type.
In such a case, it performs silent truncation:
```
> fromIntegral (300 :: Word) :: Word8
44
```

Avoid general coercion functions but write specific ones instead, as long as the type of the result is bigger than the type of the input.

```
word32ToWord64 :: Word32 -> Word64
word32ToWord64 = fromIntegral -- Safe because Word64 is bigger than Word32
```

Prefer to use functions with non-parametric types and/or functions that fail loudly, like these:

* [`naturalToInteger :: Natural -> Integer`](https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Natural.html#v:naturalToInteger) 
* [`naturalToWord :: Natural -> Maybe Word`](https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Natural.html#v:naturalToWordMaybe)
* [`toIntegralSized`](http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Bits.html#v:toIntegralSized)


Witness the trail of destruction:

* [Bug in `System.IO.hWaitForInput`](http://haskell.1045720.n5.nabble.com/Deprecating-fromIntegral-tt5864578.html) because of `fromIntegral`
* [Bug in cryptography-related code](https://github.com/haskell-crypto/cryptonite/issues/330) because of `fromIntegral`


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


### Partial functions

#### `toEnum`



### Confusing functions

#### `until`

Really confusing, use 'when' instead.

### Functions with unexpected performance characteristics

#### `nub`

O(n^2), use [`ordNub`](https://github.com/nh2/haskell-ordnub) instead

Trail of destruction:
https://gitlab.haskell.org/ghc/ghc/-/issues/8173#note_236901

#### `foldl`

Lazy. Use foldl' instead.

#### `sum` and `product`
Lazy accumulator, but is fixed as of [GHC 9.0.1](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4675).



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





