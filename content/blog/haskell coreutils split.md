Title: Haskell coreutils - split
Date: 2020-05-25
Category: Programming
Tags: computers, programming, haskell, coreutils
Status: published
Summary: Implementing split in Haskell

# Introduction

I've implemented a couple Unix core utilities in Haskell, and want to continue a series
of posts going through the details - starting with simple programs like `cat`, `seq`,
and `which`, and then moving on towards more feature-rich programs like `uniq`, `tr` and
maybe `grep`.

You can find the full source from this post
[here](https://github.com/Gandalf-/coreutils/blob/master/Coreutils/Split.hs).
Let's implement `split` in Haskell!

# Background

`split` is the original counterpart to `cat`, and comes all the way from Version 3 of
AT&T Unix. Rather than concatenating files, `split` breaks one file into multiple files.
The predicate for this could be number of bytes, number of lines, or the number of
output files. Suppose we have a 50MB PDF that we want to email to someone (some better
file sharing system isn't available), and our email system only supports attachments of
10MB. We can use `split` to break the PDF up, and our friend can use `cat` to
reconstruct it. For example:

Our PDF, 50MB.

```
I leaf@elm ~> ls -lh paper.pdf
-rw-rw-r-- 1 leaf leaf 50M May 25 12:16 paper.pdf
```

Use `split` to break the file into 10M chunks and prefix the output files with
`chunks-`.

```
leaf@elm ~> split --bytes 10M paper.pdf chunks-
leaf@elm ~> ls -lh chunks*
-rw-rw-r-- 1 leaf leaf 10M May 25 12:20 chunks-aa
-rw-rw-r-- 1 leaf leaf 10M May 25 12:20 chunks-ab
-rw-rw-r-- 1 leaf leaf 10M May 25 12:20 chunks-ac
-rw-rw-r-- 1 leaf leaf 10M May 25 12:20 chunks-ad
-rw-rw-r-- 1 leaf leaf 10M May 25 12:20 chunks-ae
```

Reconstruct the file using `cat` and verify its content.

```
leaf@elm ~> cat chunks-* > reconstructed.pdf
leaf@elm ~> sha1sum paper.pdf reconstructed.pdf
49886561f8e26ed5e2ae549897a28aaab44881bd  paper.pdf
49886561f8e26ed5e2ae549897a28aaab44881bd  reconstructed.pdf
```

Seem useful? Let's implement it in Haskell so we have a acceptably fast cross-platform
implementation, and for fun.

# Modules and Imports

The top of the file contains the module definition and imports. We'll skip going through
the imports for now, but reference them as we move through the file. To follow along
with the examples, you can put this header in a file and load it from `ghci` with
`:load split.hs`. The overloaded strings extension allows us to treat character literals
as other 'string-like' types; in this case ByteStrings.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- split
--
-- break a file into chunks by line count, byte count, or figure out the size based on a
-- required number of output files

import           Control.Monad
import qualified Data.ByteString.Lazy  as L
import           Data.Char
import           Data.Int
import           Data.List
import           System.Console.GetOpt
import           System.Exit
import           System.IO
import           Text.Read
```

# Data Flow

If we look at the essence of `split`, it has one function: splitting files, and can do
this three ways: by byte range, by line range, or by some byte range to produce a
particular number of output files. We can call this the `Runtime`, and express it in
Haskell like so.

```haskell
data Runtime =
    -- what kinds of splits are we doing?
          RunBytes Int
        | RunLines Int
        | RunChunk Int
    deriving (Show, Eq)
```

Our option parsing will determine which runtime to use, and whatever other special flags
should be enabled. The user doesn't provide the exact output files for `split`; they're
determined internally. The flags not related to the runtime have to do with influencing
what these output filenames look like. They have three parts: a prefix (default `x`), a
body (determined by split, sorted lexicographically), and an optional additional suffix.

```haskell
data Options = Options
        { optSuffixLength :: Int     -- body width
        , optExtraSuffix  :: String  -- extra suffix
        , optNumeric      :: Bool    -- use numbers in body
        , optRuntime      :: Runtime -- lines, bytes, or chunks
        }
```

A high level view of our implementation is:

1. Parse arguments to determine the runtime and additional flags
2. Use the options to produce a stream of output filenames
3. Start splitting the input file, using the runtime, into the output files

Each of our runtimes has the same prototype. Take a file, the value for your runtime
(number of bytes, number of lines, number of chunks), a stream of output filenames, and
produce an IO side effect. Like the majority of Unix utilities, `split` can read from a
filename, `stdin`, or `stdin` again if the filename is `-`. To hide this from our
runtimes, we define a `File` type which has a handle and potentially a size. This size
will be useful for an optimization later, but isn't available for `stdin`.

```haskell
data File = File Handle (Maybe Int)

splitBytes :: File -> Int -> [FilePath] -> IO ()
splitLines :: File -> Int -> [FilePath] -> IO ()
splitChunk :: File -> Int -> [FilePath] -> IO ()

runSplit :: Options -> File -> String -> IO ()
-- switchboard, and create the output filename stream
runSplit (Options s e n r) file prefix =
        case r of
            RunBytes v -> splitBytes file v filenames
            RunLines v -> splitLines file v filenames
            RunChunk v -> splitChunk file v filenames
    where
        filenames = filenameGenerator prefix n s e
```

# Bytes

Splitting by bytes is the simplest of the runtimes. All we're doing here is grabbing `n`
bytes, writing them to the next output file, and repeating if there's more to read. Our
implementation of `filenameGenerator` shouldn't ever run out of values, but GHC doesn't
know that with the types provided. To avoid a hole in the pattern matching or fussing
with additional libraries like `Data.InfList`, we'll provide a match and error message.

Using `Data.ByteString.Lazy` means that our memory usage should stay constant and fairly
low when dealing with large files or `stdin`, and that binary files will be no issue. In
files whose size isn't divisible by our byte range, the remaining bytes will spill over
into one more file that will be smaller than the rest.

```haskell
outOfFilenames :: IO ()
outOfFilenames = die "split could not generate any more output filenames"

splitBytes :: File -> Int -> [FilePath] -> IO ()
-- simplest split, just by byte ranges
splitBytes _ _ [] = outOfFilenames
splitBytes f@(File h _) n (fn:fs) = do
        L.hGet h n >>= L.writeFile fn
        hIsEOF h   >>= flip unless (splitBytes f n fs)
```

# Lines

Working with lines are a little more complex, but not much. Here we want to turn our
stream of bytes into a stream of lines. Next, batch the lines into groups of the
appropriate size, reconstruct the content by inserting newlines again, and write the
group out as a result. We're finished when we've consumed all the lines in the file.

For very large ranges of lines, appending a subgroup of lines into the output file would
be more memory efficient, but doesn't seem worth the complexity at the moment. As is,
using `Data.ByteString.Lazy` and its function for split still allows us to maintain our
lazy reading of the file and keep memory usage constant in the number of lines required
for an output file.

```haskell
splitLines :: File -> Int -> [FilePath] -> IO ()
-- create a stream of lines, group them, write them out
splitLines (File h _) n paths =
        L.split newline <$> L.hGetContents h >>= go paths
    where
        go :: [FilePath] -> [L.ByteString] -> IO ()
        go [] _ = outOfFilenames
        go _ [] = pure ()
        go (fn:fs) bs = do
            let elements = take n bs
            L.writeFile fn $
                if length elements == n
                    -- we're somewhere in the middle of the file
                    then L.snoc (L.intercalate "\n" elements) newline

                    -- don't tack an extra newline on the end of the file
                    else L.intercalate "\n" elements
            go fs (drop n bs)

        newline = 10
```

# Chunks

Writing chunks means that the user has provided the number of output files they'd like,
but not how large each of those files should be. An implicit requirement here is that
the output files are all roughly the same size.

Our job is to determine size of the output files for them. For normal files, it's easy
to get the size of a file without reading it with a `stat` call. However for `stdin`, we
will have to read everything into memory. We'll have to determine the size and (lazily)
get the content of the file different ways, but the work after that will be the same.

1. Get enough bytes from the lazy bytestring for the chunk, write it into the file.
2. When we're on the last chunk, write everything remaining into the file.

Suppose we have a a 20 byte file, and the user requests 3 chunks. To make this happen,
we'll produce chunks of size: 6, 6, and 8. The last chunk picks up the remainder rather
than spilling over into an additional file like in `splitBytes`.

```haskell
chunkWriter :: Int64 -> [FilePath] -> Int64 -> L.ByteString -> IO ()
chunkWriter _ [] _ _ = outOfFilenames
chunkWriter i (fn:fs) chunkSize bs
        -- last iteration, it gets everything remaining
        | i == 1    = L.writeFile fn bs

        -- other iteration, write our chunk and continue
        | otherwise = do
            L.writeFile fn $ L.take chunkSize bs
            chunkWriter (i - 1) fs chunkSize $ L.drop chunkSize bs
```

We've already handled whether a file size exists for our input file with our `File`
type, so our implementation here can just check for that information. Handling this
earlier prevents more code having to worry about calling `hGetSize`, or equivalent, on a
handle that doesn't support stating like `stdin`. In the case of a pre-known file size,
the implementation becomes very similar to `splitBytes` except for the handling of
remainder bytes in the last chunk. `fromIntegral` is necessary to convert the `Int`
provided by the user into an `Int64` for use by `Data.ByteString.Lazy`.

```haskell
splitChunk (File h Nothing) n paths = do
-- size is unknown, have to read it all to get the length
        bs <- L.hGetContents h
        let chunks    = fromIntegral n
            fileSize  = L.length bs
            chunkSize = fileSize `div` chunks
        chunkWriter chunks paths chunkSize bs

splitChunk (File h (Just s)) n paths = do
-- we can skip reading everything into memory since we already know the size
        bs <- L.hGetContents h
        chunkWriter chunks paths chunkSize bs
    where
        chunks    = fromIntegral n
        chunkSize = fileSize `div` chunks
        fileSize  = fromIntegral s
```

# Filename Generation

Generating suitable output filenames is the main complexity in `split` besides actually
splitting files. We're subject to a couple constraints in addition to the user provided
options:

1. Every filename generated must be lexicographically greater than the previous. Without
   this, the output files won't be sorted by shell globbing and the original file will
   be a pain to reconstruct. We'd have all the pieces, but not the ordering.

2. The stream of filenames must be infinite, even if we have to bend the user's
   arguments to make it happen. Suppose the user specifies a prefix width of 2. This
   only provides `26 ^ 2 = 676` filenames. If the input is
   `split -b 1K 100-mb-file.pdf`, we'd run out of filenames.

3. Output filenames shouldn't be longer than necessary.

To satisfy these constraints, we need permutations (order matters, compared to
combinations), in such a way that we can add more permutations without breaking
ordering. An infinite list of permutations can be had by generating permutations of size
`n`, then `n + 1`, then `n + 2`, etc. `replicateM` can give us specific permutations,
and limiting the selections to one less than all the options can provide us the wiggle
room to maintain ordering. Let's go through an example in a world where the alphabet is
only 3 letters: A, B, C.

Here are the permutations of length 2 for 'abc'

```haskell
*Coreutils.Split> replicateM 2 "abc"
["aa","ab","ac","ba","bb","bc","ca","cb","cc"]
*Coreutils.Split> isSorted xs = xs == sort xs
*Coreutils.Split> isSorted $ replicateM 2 "abc"
True
```

However, if we try to tack permutations of length 3 onto the end, we'll break ordering.

```haskell
*Coreutils.Split> replicateM 2 "abc" <> replicateM 3 "abc"
["aa","ab","ac","ba","bb","bc","ca","cb","cc","aaa","aab","aac","aba",
"abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca",
"bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
*Coreutils.Split> isSorted $ replicateM 2 "abc" <> replicateM 3 "abc"
False
```

An easy solution is available: if we filter results that begin with the last character
in our alphabet, we can use it as a prefix for all later permutations and guarantee
ordering. This continues to work even when additional 'permutation groups' are added.

```haskell
*Coreutils.Split> let g n = filter (\i -> head i /= 'c') $ replicateM n "abc"
*Coreutils.Split> g 2
["aa","ab","ac","ba","bb","bc"]
*Coreutils.Split> g 2 <> map (\i -> 'c' : i) (g 3)
["aa","ab","ac","ba","bb","bc","caaa","caab","caac","caba","cabb", "cabc",
"caca","cacb","cacc","cbaa","cbab","cbac","cbba","cbbb","cbbc","cbca","cbcb","cbcc"]
*Coreutils.Split> isSorted $ g 2 <> map (\i -> 'c' : i) (g 3)
True
```

Here's what that looks like all together and handling the options we're exposing to the
user. We'll start with the width provided, but increase it as necessary. `numeric`
determines whether we're using letters or numbers in our alphabet. In either case, the
idea is the same: filter out results that start with the last element in our alphabet so
we can maintain ordering. `prefix` and `suffix` allow the user more control over what
the results look like.

```haskell
filenameGenerator :: String -> Bool -> Int -> String -> [String]
-- ^ lazy list of filenames that conform to these options. 9 and z aren't included
-- in the 'main' body of results since they're needed to prefix the next group so
-- we maintain ordering
filenameGenerator prefix numeric width suffix =
        initial <> more
    where
        initial =
            map (\i -> prefix <> i <> suffix)
            $ filter (\i -> head i /= next)
            $ replicateM width characters
        more = filenameGenerator (prefix <> [next]) numeric (width + 1) suffix

        characters
            | numeric   = concatMap show ([0..8] :: [Integer])
            | otherwise = ['a'..'y']

        next = last characters
```

# Option Parsing and Running

With the core implementation handled, all that's left is to parse the user's command
line arguments into our `Options` type and wire everything together. We'll use
`System.Console.GetOpt` from the standard library for parsing. It's simple and
expressive enough for our needs here. To use it, we'll define our default values and
describe the options with help text conforming to the prototype `getOpt` requires. I've
elided most of the option definitions here, but you can see them all
[here](https://github.com/Gandalf-/coreutils/blob/master/Coreutils/Split.hs).

```haskell
defaults :: Options
defaults = Options
        { optSuffixLength = 2
        , optExtraSuffix  = ""
        , optNumeric      = False
        , optRuntime      = NoRuntime
        }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option "a" ["suffix-length"]
        (ReqArg
            (\arg opt -> case readMaybe arg of
                Nothing  -> Left $ "could not parse " <> arg <> " as a number"
                (Just n) -> Right opt { optSuffixLength = n })
            "N")
        "generate suffixes of length N (default 2)"
    ...
```

With our options defined, we're ready to write our `splitMain` function. It will only
require the output of `getArgs` to run, i.e. we could define `main` as
`getArgs >>= splitMain`.

The first thing to do is parse options. `getOpt` returns a triple:

1. Successfully parsed options
2. Arguments that were not parsed as options
3. Errors discovered during parsing, such as providing a flag that requires an
   argument without an argument

Assuming success, we need to separate the input file from prefix if provided. We can use
pattern matching to get the optional behavior that `split` provides.

1. If nothing is provided, the file is `stdin` and the prefix is 'x'
2. If one thing is provided, it's used as a filename and the prefix is 'x'
3. If two things are provided, the first is used as a filename and the second as a prefix
4. If anything else is provided, it's an error

Additionally, this where we'll handle the `- == stdin` conversion and collect a file
size if possible. All the code after this point deals only with a `File` data type, and
doesn't need to know whether it's working against `stdin` or a normal file.

```haskell
splitMain :: [String] -> IO ()
-- parse arguments and check for obvious errors
splitMain args = do
        let (actions, files, errors) = getOpt RequireOrder options args

        unless (null errors) $ do
            mapM_ putStr errors
            exitFailure

        (prefix, file) <- case files of
            []           -> return ("x", File stdin Nothing)
            [fn]         -> (,) "x" <$> getHandleAndSize fn
            [fn, prefix] -> (,) prefix <$> getHandleAndSize fn
            _            -> die "split cannot operate on more than one file at a time"

        case foldM (flip id) defaults actions of
            Left err   -> die err
            Right opts -> runSplit opts file prefix
    where
        getHandleAndSize :: FilePath -> IO File
        getHandleAndSize "-" = pure $ File stdin Nothing
        getHandleAndSize fn = do
                h <- openBinaryFile fn ReadMode
                size <- fromIntegral <$> hFileSize h
                pure $ File h (Just size)
```

A bit of spelunking in GHCI will show what's going on with
`foldM (flip id) defaults action`.

```haskell
*Coreutils.Split> let (a, f, e) = getOpt RequireOrder options ["-d", "file"]
*Coreutils.Split> f
["file"]
*Coreutils.Split> e
[]
*Coreutils.Split> :t a
a :: [Options -> Either String Options]
*Coreutils.Split> :t foldM (flip id) defaults
foldM (flip id) defaults
  :: (Foldable t, Monad m) => t (Options -> m Options) -> m Options
*Coreutils.Split> :t foldM (flip id) defaults a
foldM (flip id) defaults a :: Either String Options
```

It's using fold to apply the default options to each of the functions we defined in
`options` to build up a 'filled in' `Options` result. If we have a match, the value from
`actions` is used, otherwise the default value is used. The result of all this is either
an error string or useable `Options` that we can pass to `runSplit`. And that's it!

# Conclusion

We have a fully functional implementation of `split` in Haskell! We identified the core
functionality, created data types to represent it with `Runtime` and `Options`, and
implemented the core functionality using lazy IO and abstracted input. To tie it all
together, we built a helper function for creating an infinite list of lexicographically
ordered filenames, defined options and how to parse them, and wrote a main function to
kick everything off.
[Here](https://github.com/Gandalf-/coreutils/blob/master/Coreutils/Split.hs)'s all the
code together.
