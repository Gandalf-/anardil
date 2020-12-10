Title: Haskell coreutils - tee
Date: 2020-12-10
Category: Programming
Tags: computers, programming, haskell, coreutils
Status: published
Summary: Implementing tee in Haskell

# Introduction

I've implemented some [Unix core utilities in Haskell](https://github.com/Gandalf-/coreutils), and want to start a series of
posts going through the details - starting with simple programs like `cat`,
`seq`, and `which`, and then moving on towards more featureful programs like
`uniq`, `tr` and maybe `grep`.

You can find the full source from this post
[here](https://github.com/Gandalf-/coreutils/blob/master/Coreutils/Tee.hs).
Let's implement `tee` in Haskell!

# Background

What does `tee` do? From the man page, "read from standard input and write to
standard output and files". Seems simple enough; `tee` is like `cat` except
that it additionally writes whatever passes between `stdin` and `stdout` to any
number of files along the way. Like the majority of coreutils, this is done in
a streaming fashion for performance and to reduce memory usage. It's
unacceptable, for instance, to read all of stdin, then write it to stdout and
each output file in turn. We need to *stream* the data to each output, or sink,
as it becomes available.

```text
I leaf@elm ~> echo hello world | tee 1 2 3
hello world

I leaf@elm ~> cat 1 2 3
hello world
hello world
hello world
```

Let's sketch out the program in types to see what we need. We'll use this as a
reference for each section below:

```haskell
teeMain :: [String] -> IO ()
-- ^ parse arguments with defaults, look for errors, call runTee

runTee :: Options -> [FilePath] -> IO ()
-- ^ open handles for each filepath according to provided options, call tee

tee :: [Handle] -> IO ()
-- ^ read from stdin, write to each provided handle and stdout
```

Let's work our way top down, starting with argument parsing, then down to
our runtime, and lastly the business logic.

# Modules and Imports

Our main concern is streaming data, for which Haskell has a couple libraries.
We'll be using `streaming` and `streaming-bytestring` which provide
`Data.ByteString.Streaming`. ByteStrings are the way to go since `tee` must
behave itself with binary input, and besides we don't need to concern ourselves
with the content of stdin means to move it around. `System.Console.GetOpt` will
handle argument parsing, while the other `System` and `Control` libraries
provide some basics: `bracket`, `unless`, `die`, `openBinaryFile`, open flags,
and `Handle`. We'll see how these are each used in the following sections.

```haskell
module Coreutils.Tee where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Streaming as Q
import           System.Console.GetOpt
import           System.Exit
import           System.IO
```

# Arguments - teeMain

BSD `tee` has just two options

- `-a` to append to output files rather than overwriting them
- `-i` to ignore SIGINT

GNU tee has some more options related to error path behavior, but let's ignore
those and BSD's `-i` for the time being. This leaves us with three things to do
in our argument parsing:

- look for help flags to show usage
- look for `-a` or `--append` to indicate append mode for writing
- collect everything else as output file names

`System.Console.GetOpt` provides a simple pattern to describe this exactly, we
provide a data type describing our options (in this case, just one), the
defaults, and some help text and it'll figure out the details internally.

```haskell
newtype Options = Options { optMode :: IOMode }

defaults :: Options
defaults = Options { optMode = WriteMode }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "a" ["append"]
        (NoArg
            (\opt -> Right opt { optMode = AppendMode }))
        "append to given files, do not overwrite"

    , Option "h" ["help"]
        (NoArg
            (\_ -> Left $ usageInfo "tee" options))
        "show this help text"
    ]
```

`getOpt` automatically tracks which arguments aren't parsed and provides those
separately, perfect for our usecase. Let's plug this all together in our pseudo
main function.

```haskell
teeMain :: [String] -> IO ()
teeMain args = do
        unless (null errors) $
            die $ unlines errors

        either die (`runTee` filenames) $
            foldM (flip id) defaults opts
    where
        (opts, filenames, errors) = getOpt RequireOrder options args
```

The real driver here is:

```haskell
*Coreutils.Tee> :t getOpt
getOpt
  :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String])

*Coreutils.Tee> let someArgs = ["-a", "out.txt"]
*Coreutils.Tee> :t getOpt RequireOrder options someArgs
getOpt RequireOrder options someArgs
  :: ([Options -> Either String Options], [String], [String])
```

where `options` describes the flags we're looking to parse, and `args` are the
command line arguments, as per `System.Environment.getArgs`. Once parsed, we
check for errors, apply defaults with `foldM (flip id) defaults opts`, and run.
The `foldM` has a bit going on, let's break that down by specializing the
arguments one at a time.

```haskell
*Coreutils.Tee> :t foldM
foldM
  :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b

*Coreutils.Tee> :t foldM (flip id)
foldM (flip id)
  :: (Foldable t, Monad m) => b -> t (b -> m b) -> m b

*Coreutils.Tee> :t foldM (flip id) defaults
foldM (flip id) defaults
  :: (Foldable t, Monad m) => t (Options -> m Options) -> m Options
```

What about `opts`? We can see the types align with `getOpt`'s first tuple if
our Foldable is a list and Monad is Either. This makes sense from a higher
level too; we have multiple "combine-able" operations (parsing flags) that can
succeed (provide an `Option`) or fail (provide a `String` error message). All
together, this executes the parsing functions `opts` in turn to build an
`Either String Options`, filling in the blanks with our defaults as necessary.

When we're all done, we have `Options` and a list of everything else not parsed
which we can use as the list of output filenames.

# Resources - runTee

Let's take a look back and see what we're supposed to do next.

```haskell
runTee :: Options -> [FilePath] -> IO ()
-- ^ open handles for each filepath according to provided options, call tee
```

So we have our options and filenames, and need to convert those into handles to
call the next layer. To properly manage our resources (these handles), we need
to close them too. Breaking these steps out in a `do` block would work most of
the time, but would leak if we hit an exception. On Linux, this isn't such a
big deal - the process exiting will close all the handles. However on Windows
(which we can support for free thanks to Haskell's IO libraries), not closing
the handles can mean that data doesn't get written. To that point, Haskell uses
exceptions to communicate IO errors, the exact type of errors we're likely to
encounter opening and writing to files. Luckily, `bracket` is perfect for this
situation; let's check it out.

```haskell
*Coreutils.Tee> :t bracket
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
```

Provided some IO computation that produces resources, a function that uses
those resources, and a function that releases those resources, `bracket` will
run everything together, ensuring that the resources are released even if
there's an exception while they're being used.

```haskell
runTee :: Options -> [FilePath] -> IO ()
runTee o fs =
        bracket acquire release tee
    where
        acquire = mapM (`openBinaryFile` optMode o) fs
        release = mapM_ hClose
```

Pretty easy!

# Business Logic - tee

So now we have our collection of handles, time to use them to do some real
work. Let's see it all together, then break it down.

```haskell
tee :: [Handle] -> IO ()
-- build up n computations that copy the stream and write it a file
tee = Q.stdout . foldr (\h -> Q.toHandle h . Q.copy) Q.stdin
```

`Data.ByteString.Streaming` usage works right to left, where the right side is
the source of the stream, and the left side is the sink, where the data ends
up. The space between is where we can mutate the stream. The simplest `tee` is
with no files, in which it's just a simplified `cat` that only reads from
`stdin`. To describe that with these streams, that'd be:

```haskell
*Coreutils.Tee> let cat = Q.stdout Q.stdin
*Coreutils.Tee> :t cat
cat :: Control.Monad.IO.Class.MonadIO m => m ()
```

Take everything from stdin and stream it to stdout. For our purposes, `m` is
`IO`, nothing else is needed here. We can specialize our types to see this
ourselves, and that we're going to fit into the prototype we sketched out
initially for `tee`.

```haskell
*Coreutils.Tee> let cat = Q.stdout Q.stdin :: IO ()
*Coreutils.Tee> :t cat
cat :: IO ()
```

Now, for writing streams to handles we have `Q.toHandle`, but this has a
problem - it acts like a sink, consuming all of the input stream. This won't
work, since the input from `stdin` will never make it to `stdout`. We can't
read the stream twice either; for a file we could read everything twice, it
would just be wasteful, but for `stdin` it's not possible - the data only
exists once.

The library has something for us though: `Q.copy` forks a stream, allowing you
to do two separate, independent computations on it. Internally, this is
essentially sending the chunks that make up the input stream two different
places, creating two streams from one.

Let's build up a `cat` the preserves the input stream beyond writing to stdout
rather than consuming it.

```haskell
*Coreutils.Tee> :t Q.copy
Q.copy
  :: Monad m => Q.ByteString m r -> Q.ByteString (Q.ByteString m) r

*Coreutils.Tee> :t Q.copy Q.stdin
Q.copy Q.stdin
  :: Control.Monad.IO.Class.MonadIO m =>
     Q.ByteString (Q.ByteString m) ()

*Coreutils.Tee> :t (Q.stdout . Q.copy) Q.stdin
(Q.stdout . Q.copy) Q.stdin
  :: Control.Monad.IO.Class.MonadIO m => Q.ByteString m ()
```

While we're here thinking about stdout, let's note that `Q.stdout` isn't doing
anything magical compared to `Q.tohandle`, just sparing us some typing. This is
useful, because it let's us treat stdout as "just another output", the same as
the handles we're creating.

```haskell
*Coreutils.Tee> :t Q.toHandle stdout
Q.toHandle stdout
  :: Control.Monad.IO.Class.MonadIO m => Q.ByteString m r -> m r

*Coreutils.Tee> :t Q.stdout
Q.stdout
  :: Control.Monad.IO.Class.MonadIO m => Q.ByteString m r -> m r
```

With our stream copying ability, we can create a waterfall of streams! stdin to
the first handle + new stream 1, new stream 1 to the second handle + new stream
2, and so on until the last stream, which just goes to stdout.

Good old `foldr` matches this pattern well; take some initial value, run a
computation on it with the first input to produce an output, then use that as
the new initial value for the second input value, and so on.

```haskell
*Coreutils.Tee> foldr (+) 0 [1..10]
55
```

In streaming pseudo-code, we want `foldr writeTohandleAndCopy stdin handles`,
which will produce a final stream, which we dump into stdout to finish
everything off. In terms of real code, that works out to what we had before!

```haskell
tee :: [Handle] -> IO ()
-- full
tee handles = Q.stdout . foldr (\h -> Q.toHandle h . Q.copy) Q.stdin $ handles

-- point free
tee = Q.stdout . foldr (\h -> Q.toHandle h . Q.copy) Q.stdin
```

# Conclusion

We've built a high performance, elegant `tee` replacement using the
`streaming-bytestring` library. We looked at simple argument parsing including
defaults and error handling, proper resource management in the face of
exceptions, how to describe our program in terms of streams, and how to
manipulate those streams in order to write to multiple sinks. And all in ~60
lines of code!
[Here](https://github.com/Gandalf-/coreutils/blob/master/Coreutils/Tee.hs)'s
all the code together.
