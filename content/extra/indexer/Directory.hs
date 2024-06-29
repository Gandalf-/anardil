module Indexer.Directory where

data FileElement = FileElement
        { _fname :: String
        , _fpath :: FilePath      -- ^ full path to the file
        , _fsize :: Integer       -- ^ size in bytes
        , _ftime :: Integer       -- ^ how many seconds old is this file?
        , _ficon :: Maybe String  -- ^ URI for appropriate icon
        , _fexec :: Bool          -- ^ is this file executable?
        }
    deriving (Show, Eq)

data DirElement = DirElement
        { _dname    :: String
        , _dpath    :: FilePath
        , _children :: [DirectoryElement]
        }
    deriving (Show, Eq)

data DirectoryElement =
          File      FileElement
        | Directory DirElement
        | ParentDir DirElement
    deriving (Show, Eq)

instance Ord DirectoryElement where
    -- compare files and directories by name
    compare (File a)      (File b)      = compare (_fname a) (_fname b)
    compare (Directory a) (Directory b) = compare (_dname a) (_dname b)

    -- directories are always before files
    compare (File _)      (Directory _) = GT
    compare (Directory _) (File _)      = LT

    -- parent directories are before everything
    compare _          (ParentDir _)    = GT
    compare (ParentDir _) _             = LT
