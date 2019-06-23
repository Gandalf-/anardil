Title: Haskell coreutils - cat
Date: 2019-03-30
Category: Programming
Tags: computers, programming, haskell, coreutils
Status: published
Summary: Implementing cat in Haskell

# Introduction

I've implemented a couple Unix core utilities in Haskell, and want to start a
series of posts going through the details - starting with simple programs like
`cat`, `seq`, and `which`, and then moving on towards more featureful programs
like `uniq`, `tr` and maybe `grep`.

So, let's implement `cat` in Haskell!


# Background

`cat` is conceptually simple; it concatenates a series of files. It doesn't
accept any flags, and has only a little dynamic behavior - if there aren't any
files provided from the command line, it reads from `stdin`. If a series of
files are provided and there's an error reading one of them, it's reported but
the rest of the files are processed. `cat` exits with failure if there were any
problems.

# Module and Imports

The top of the file contains the module definition and imports. Since this is
going to be an executable, not a library, we use `module Main where`. We'll
skip going through the imports for now, but reference them as we move through
the file. To follow along with the examples, you can put this header in a file
and load it from `ghci` with `:load cat.hs`.

```haskell
module Main where

-- cat
--
-- read files from the command line or echo stdin
-- soldiers on when some files do not exist, but reports failure at the end

import           Control.Exception  (IOException, try)
import           Control.Monad      (when)
import           Data.Either        (isLeft)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.IO          (hPutStrLn, stderr)
```

# Data Flow

Before jumping in to find some functions that can read and print file content,
let's think about the 'flow' of execution for `cat`. It takes arguments,
attempts to convert those into file names, extracts the file content, then
prints it. What about errors? Conceptually, we can think of each argument
turning into either file content, or an error. Either way, we print out the
content or error at the end.

When we're done, we should have something like this:

```haskell
main :: IO ()
main = getArgs >>= collect >>= display
```

Haskell has a great builtin data type for this situation: `Either`. Translating
our conceptual view of `cat` to Haskell looks something like this.

```haskell
type Argument    = String
type FileContent = String

collect :: [Argument] -> IO [Either IOException FileContent]
collect = undefined

display :: [Either IOException FileContent] -> IO ()
display = undefined
```

# Collect

We can fill in the `undefined` for collect with some of the imports from
before. How are we going to turn a file name into an IOException or
FileContent? Let's build it from the bottom up. To read a file, we need
`readFile`

```haskell
> :t readFile
readFile :: FilePath -> IO String
```

The problem is that `readFile` throws an IOException on failure. `try` allows
us to capture the exception for handling. This is conceptually what we want;
either the result of `readFile`, or the exception it threw.

```haskell
> :t try
try :: GHC.Exception.Exception e => IO a -> IO (Either e a)

> :t try . readFile
try . readFile
  :: GHC.Exception.Exception e => FilePath -> IO (Either e String)
```

To apply these to each argument, we can use `mapM`, which is like `map`, but works
on a sequence of `Monad m` items. It has a pretty abstract type signature, but
a type check shows that it's doing what we want.

```haskell
> :t mapM
mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)

> :t mapM (try . readFile)
mapM (try . readFile)
  :: (Traversable t, GHC.Exception.Exception e) =>
       t FilePath -> IO (t (Either e String))
```

Putting it all together, we can define collect as

```haskell
collect :: [Argument] -> IO [Either IOException FileContent]
collect = mapM (try . readFile)
```

# Stdin

What if we're not given any arguments? We need to read from `stdin`. This case
is a bit simpler, since there isn't any reasonable possibility for an error. We
can ignore the input (since there isn't any!) and get the content from stdin
and print it out. `getContents` from the Prelude does just what we want. It
reads from stdin until EOF, and returns an `IO String`. We'll use `putStr`
instead of `putStrLn`, since the input will already have a newline.

```haskell
display :: [Either IOException FileContent] -> IO ()
display [] = getContents >>= putStr
```

# Files

The other case is when we do have some 'Error or FileContent' to work with. We
want to print the error or file content either way, but errors should go to
`stderr`, not `stdout`. At the end, if there were any errors, we want to set
the exit code appropriately.

Our printing function needs to handle both possibilities:

```haskell
toConsole (Left exception) = hPutStrLn stderr $ show exception
toConsole (Right content)  = putStr content
```

And display will apply it to each argument, and handle exiting correctly.
`any isLeft files` is doing the work of answering "were there any exceptions?".

```haskell
display :: [Either IOException FileContent] -> IO ()
display files = do
        mapM_ toConsole files
        when (any isLeft files) exitFailure
```

# Main

How do we tie everything together? If we think back to the high level data flow
at the beginning, we describe that exactly in Haskell for our `main` function.

```haskell
main :: IO ()
main = getArgs >>= collect >>= display
```


# Full implementation

Here's the full source. You can also find it
[here](https://public.anardil.net/share/code/coreutils/cat.hs).

```haskell
module Main where

-- cat
--
-- read files from the command line or echo stdin
-- soldiers on when some files do not exist, but reports failure at the end

import           Control.Exception  (IOException, try)
import           Control.Monad      (when)
import           Data.Either        (isLeft)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.IO          (hPutStrLn, stderr)

type Argument    = String
type FileContent = String

collect :: [Argument] -> IO [Either IOException FileContent]
collect = mapM (try . readFile)

display :: [Either IOException FileContent] -> IO ()
display [] = getContents >>= putStr

display files = do
        mapM_ toConsole files
        when (any isLeft files) exitFailure
    where
        toConsole (Left exception) = hPutStrLn stderr $ show exception
        toConsole (Right content)  = putStr content

main :: IO ()
main = getArgs >>= collect >>= display
```

# Conclusion

We have a fully functional implementation of `cat` in Haskell! By taking some
time to think about the flow of data, breaking down the component parts,
implementing the parts individually, then tying them together, we've produced
an elegant and correct program.
