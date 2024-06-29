module Indexer.Age where

import           Indexer.Directory


data Age = Age Integer | NoAge
    deriving (Eq)

instance Ord Age where
    -- ^ we use minimum to find the age of a directory by it's children, but want to
    -- exclude parent directories which have `NoAge`, hence the custom Ord. Maybe treats
    -- Nothing as less than (Just a)
    compare (Age _) NoAge   = LT
    compare NoAge NoAge     = EQ
    compare NoAge _         = GT
    compare (Age a) (Age b) = compare a b

instance Show Age where
    show NoAge = "-"
    show (Age t)
            | t < hour      = "minutes"
            | t < hour  * 2 = "hour"
            | t < day       = "hours"
            | t < day   * 2 = "day"
            | t < week      = "days"
            | t < week  * 2 = "week"
            | t < month     = "weeks"
            | t < month * 2 = "month"
            | t < year      = "months"
            | t < year  * 2 = "year"
            | otherwise     = "years"
        where
            year   = month * 12
            month  = week * 4
            week   = day * 7
            day    = hour * 24
            hour   = minute * 60
            minute = 60


class Ageable a where
    age :: a -> Age

instance Ageable FileElement where
    age = Age . _ftime

instance Ageable DirElement where
    age = age . _children

instance Ageable DirectoryElement where
    age (File f)      = age f
    age (Directory d) = age d
    age (ParentDir p) = age p

instance (Ageable a) => Ageable [a] where
    age [] = NoAge
    age xs = minimum $ map age xs
