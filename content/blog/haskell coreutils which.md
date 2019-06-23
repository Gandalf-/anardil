Title: Haskell coreutils - which
Date: 2019-04-14
Category: Programming
Tags: computers, programming, haskell, coreutils
Status: published
Summary: Implementing which in Haskell

# Introduction

I've implemented a couple Unix core utilities in Haskell, and want to continue
a series of posts going through the details - starting with simple programs
like `cat`, `seq`, and `which`, and then moving on towards more featureful
programs like `uniq`, `tr` and maybe `grep`.

So, let's implement `which` in Haskell!


# Background

On most operating systems (Linux, Windows, MacOS, *BSD), the `PATH` environment
variable defines which directories contain executables. `which` helps you find
an executable by searching through these directories.

On Linux, `PATH` typically looks something like this:
```
% echo $PATH
/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/sbin:/usr/local/sbin:/usr/sbin
```

A colon separated, ordered list of directory paths.

On Windows, `PATH` looks like this
```
PS C:\> echo $Env:PATH
C:\Windows\system32;C:\Windows;C:\WindowsSystem32\Wbem;C:\Windows\System32\WindowsPowerShell\v1.0;C:\Users\leaf\AppData\Roaming\local\bin
```

A semicolon separated, ordered list of directory paths.

The goal is the same for both, parse the variable output into a list, search
each of these directories in order, and return the first match. This is
mirroring the behavior that your shell (`sh`, `zsh`, `powershell`) performs to
determine what executable to run based on the command names you provide.

It's reasonable for executables with the same name to appear in different
places in `PATH`, which makes the ordering important. For instance, if you had
a custom `cat` executable on Linux, you could place that in `/usr/local/bin`,
and it would shadow the system copy in `/bin`.


# Module and Imports

The top of the file contains the module definition and imports. Since this is
going to be an executable, not a library, we use `module Main where`. We'll
skip going through the imports for now, but reference them as we move through
the file. To follow along with the examples, you can put this header in a file
and load it from `ghci` with `:load which.hs`. This
[GitHub repository](https://github.com/Gandalf-/coreutils/) has a working
`.cabal` file as an example.

```haskell
module Main where

-- which
--
-- look for the arguments on $PATH

import           Control.Monad      (filterM, when)
import           Data.List.Split    (splitOn)
import           Data.Maybe
import           System.Directory
import           System.Environment (getArgs, getEnv)
import           System.Exit        (exitFailure)
import           System.Info        (os)
```


# Data Flow

Our input comes from two places, the command line and the environment. `PATH`
expands to a list of directories that may contain the target file. We need to
maintain the following constraints:

- only consider directories on `PATH` that exist
- there must be a file with the same name in the directory
- the file match we found must be executable
- if no match can be found, continue, but exit with failure when finished

After expanding `PATH`, we'll be able to use filters to apply each of the
constraints.


# Parsing PATH

Lets start with parsing `PATH`. To read the variable, we need two functions to
start:

- `getEnv :: String -> IO String`
- `splitOn :: Eq a => [a] -> [a] -> [[a]]`

Composing these with `<$>` shows we're on the right track - dividing
`PATH` into directories.

```haskell
*Main> take 5 . reverse . splitOn ":" <$> getEnv "PATH"
["/usr/local/games","/usr/games","/bin","/sbin","/usr/bin"]
```

What if some of these directories are invalid? Or point to a network location
that's not accessible right now? We need to filter out directories that don't
exist. Since checking for directory existance is necessarily dependant on the
outside world, we'll need `filterM` instead of `filter`.

```haskell
*Main> take 5 . reverse . splitOn ":" <$> getEnv "PATH" >>= filterM doesDirectoryExist
["/usr/local/games","/usr/games","/bin","/sbin","/usr/bin"]
```

This works for Linux, but what about Windows? We need some platform dependant
behavior and a function to wrap this up. The `os` constant from `System.Info`
gives us a plain `String` to work with. Alternatively, we could use the `CPP`
language extension and `#ifndef mingw32_HOST_OS` to make this decision at
compile time.  For this kind of simple difference, keeping the behavior present
in the code seems cleaner.

```haskell
getPaths :: IO [FilePath]
-- ^ convert the PATH variable to a list of valid directories
getPaths =
        splitOn separator <$> getEnv "PATH"
            >>= filterM doesDirectoryExist
    where
        separator = case os of
            "mingw32" -> ";"
            _         -> ":"
```

vs

```haskell
getPaths :: IO [FilePath]
-- ^ convert the PATH variable to a list of valid directories
getPaths =
        splitOn separator <$> getEnv "PATH"
            >>= filterM doesDirectoryExist
    where
#ifndef mingw32_HOST_OS
        separator = ":"  -- POSIX
#else
        separator = ";"  -- Windows
#endif
```

# Filtering on IO

Now that we have a list of directories to consider, we can start restricting
(`filter`) and manipulating (`map`) the list to match the constraints. The
`map` and `filter` functions apply non-Monad functions to iterables. This is
what we're used to when working with pure code.

```haskell
-- even :: Integral a => a -> Bool
-- map (*2) :: Num b => [b] -> [b]

*Main> map (*2) . filter even $ [1..10]
[4,8,12,16,20]
```

Our input comes from the IO Monad, but this isn't a problem. We can use `<$>`
to apply pure functions on Monad values, like we did to split the `PATH`
variable. The tricky part is that the functions we need to apply for filtering
and mapping  produce IO values.

```haskell
executable :: FilePath -> IO Bool
doesFileExist :: FilePath -> IO Bool
```

`mapM` and `filterM` allow us to apply these functions, and `>>=` lets us bind
them together, similar to what we did with `.` and `map` + `filter`.

```haskell
filterM :: Applicative m => (a -> m Bool) -> [a] -> m [a]
mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
```

The flow of data will be:

- a list of directories
- a list of filepaths (directory + file)
- a list of filepaths that exist
- a list of executable filepaths

Translating this flow into Haskell:

```haskell
search :: String -> IO [FilePath]
-- ^ look for a file in the right directories, that's executable
-- instead of checking dir contents, create the path and check for existence
search file =
        getPaths
            >>= mapM    addFileToPath
            >>= filterM doesFileExist
            >>= filterM runnable
    where
        addFileToPath directory = pure $ pathJoin directory file
```

With this framework in place, the next step is to fill in the supporting
functions to do the real world work. `pathJoin` acts like Python's
`os.path.join()` - be platform aware and don't add unnecessary path separators.
We can't guarantee that the path names end in separators.  For example,
`/usr/bin` vs `/usr/bin/`.

```haskell
runnable :: FilePath -> IO Bool
-- ^ is this file executable? expects it to exist
runnable file = executable <$> getPermissions file

pathJoin :: FilePath -> String -> FilePath
-- ^ join a directory path and filename, platform aware
-- could have used System.FilePath.Posix (filePath) here
pathJoin []   file = file
pathJoin base file =
        if last base == separator
            then base <> file
            else base <> [separator] <> file
    where
        separator = case os of
            "mingw32" -> '\\'
            _         -> '/'
```

# Putting it all together

The hard work is done, what's left to do is tie it all together. The error
condition is when no match could be found. Otherwise, we print the first
result. `head` and `Maybe` provide a simple way to express this.

```haskell
main :: IO ()
-- ^ print the path to each argument if possible
-- if anything didn't exist, exit failure
main = do
        paths <- getArgs >>= mapM which
        when (any isNothing paths) exitFailure
        mapM_ putStrLn $ catMaybes paths


which :: String -> IO (Maybe String)
-- ^ grab the first result, if there was one
which file =
        maybeHead <$> search file
    where
        maybeHead []    = Nothing
        maybeHead (x:_) = Just x
```

`main` applies each command line argument to this function, and checks for
`Nothing`.


# Conclusion

We have a fully functional, platform independent implementation of `which` in
Haskell! We used `filterM` and `mapM` to work with IO functions in an elegant
way, and produce a pipeline that applies each constraint. `System.Info (os)`
let's us account for differences in platforms. Moreover, it takes advantage of
Haskell's lazy evaluation, and won't continue searching for a match after it's
been found.

- Linux
```text
$ stack exec which python bash
/usr/bin/python
/bin/bash
```

- Windows
```text
PS C:\> stack exec which python.exe cmd.exe
C:\Users\leaf\AppData\Local\Programs\Python\Python37-32\python.exe
C:\Windows\system32\cmd.exe
```

Thanks to [u/andersk](https://www.reddit.com/user/andersk) for pointing some
efficiency improvements!


# Full implementation

Here's the full source. You can also find it
[here](https://github.com/Gandalf-/coreutils/blob/master/which.hs).

```haskell
module Main where

-- which
--
-- look for the arguments on $PATH

import           Control.Monad      (filterM, when)
import           Data.List.Split    (splitOn)
import           Data.Maybe
import           System.Directory
import           System.Environment (getArgs, getEnv)
import           System.Exit        (exitFailure)
import           System.Info        (os)


main :: IO ()
-- ^ print the path to each argument if possible
-- if anything didn't exist, exit failure
main = do
        paths <- getArgs >>= mapM which
        when (any isNothing paths) exitFailure
        mapM_ putStrLn $ catMaybes paths


which :: String -> IO (Maybe String)
-- ^ grab the first result, if there was one
which file =
        maybeHead <$> search file
    where
        maybeHead []    = Nothing
        maybeHead (x:_) = Just x


search :: String -> IO [FilePath]
-- ^ look for a file in the right directories, that's executable
search file =
        getPaths
            >>= mapM    addFileToPath
            >>= filterM doesFileExist
            >>= filterM runnable
    where
        addFileToPath directory = pure $ pathJoin directory file


pathJoin :: FilePath -> String -> FilePath
-- ^ join a directory path and filename, platform aware
-- could have used System.FilePath.Posix (filePath) here
pathJoin []   file = file
pathJoin base file =
        if last base == separator
            then base <> file
            else base <> [separator] <> file
    where
        separator = case os of
            "mingw32" -> '\\'
            _         -> '/'


getPaths :: IO [FilePath]
-- ^ convert the PATH variable to a list of valid directories
getPaths =
        splitOn separator <$> getEnv "PATH"
            >>= filterM doesDirectoryExist
    where
        separator = case os of
            "mingw32" -> ";"
            _         -> ":"


runnable :: FilePath -> IO Bool
-- ^ is this file executable? expects it to exist
runnable file = executable <$> getPermissions file
```
