module Indexer.Size where

import           Numeric

import           Indexer.Directory

data Size = Size Integer | NoSize
    -- ^ all of this so we can use `sum`, where NoSize is effectively ignored. we could
    -- have given parent directories 0 size, but this isn't quite right, and would force
    -- us to represent empty files the same way.
    --
    -- with this, we can have '-' size and '0.0 B' size separately
    deriving (Eq)

instance Num Size where
    (+) (Size a) (Size b) = Size $ a + b
    (+) (Size a) NoSize   = Size a
    (+) NoSize   (Size b) = Size b
    (+) NoSize   NoSize   = NoSize

    fromInteger a = Size $ fromInteger a

    (*)    = undefined  -- these are easy to implement, but aren't needed
    abs    = undefined
    signum = undefined
    negate = undefined

instance Show Size where
    show NoSize = "-"

    show (Size n)
            | n < kb    = adjust n 1  <> " B"
            | n < mb    = adjust n kb <> " KB"
            | n < gb    = adjust n mb <> " MB"
            | n < tb    = adjust n gb <> " GB"
            | otherwise = adjust n tb <> " TB"
        where
            kb = 1024
            mb = 1024 * kb
            gb = 1024 * mb
            tb = 1024 * gb

            adjust :: Integer -> Integer -> String
            adjust a b = formatFloat 1 $
                (realToFrac a :: Double) / (realToFrac b :: Double)

            formatFloat c f = showFFloat (Just c) f ""


class Sizeable a where
    size :: a -> Size

instance Sizeable FileElement where
    size = Size . _fsize

instance Sizeable DirElement where
    size = size . _children

instance (Sizeable a) => Sizeable [a] where
    size = sum . map size

instance Sizeable DirectoryElement where
    size (File f)      = size f
    size (Directory d) = size d
    size (ParentDir _) = NoSize
