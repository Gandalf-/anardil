module Main where

import           Data.Char      (isAlpha, ord, toLower)
import           Data.List      (intersperse, sort)
import           Data.Maybe     (catMaybes, fromMaybe)
import           System.IO      (BufferMode (..), hSetBuffering, stdin, stdout)
import           System.Process (callCommand)


data Puzzle = Puzzle
            { secret  :: String
            , board   :: [Maybe Char]
            , guessed :: String
            }

instance Show Puzzle where

    show (Puzzle s w g) =
            concat
            [ "\n"
            , "         ", space showTitle, "\n"
            , "\n"
            , "Board:   ", space showBoard , "\n"
            , "\n"
            , "Guessed: ", space showGuessed
            , "\n"
            ]

        where showBoard = fromMaybe '_' <$> w
              showTitle = take (length g) "Hangman"
              showGuessed = sort g
              space = intersperse ' '


newPuzzle :: String -> Puzzle
newPuzzle s = Puzzle secret board guessed
    where
        board   = Nothing <$ s
        secret  = toLower <$> s
        guessed = []


play :: Puzzle -> Char -> Puzzle
play p@(Puzzle secret board guessed) c
        | Just c `elem` board = p
        | c `elem` guessed    = p
        | c `elem` secret     = correct
        | otherwise           = incorrect
    where
        correct   = Puzzle secret updatedBoard guessed
        incorrect = Puzzle secret board (c : guessed)
        updatedBoard = updateBoard board c secret


updateBoard :: [Maybe Char] -> Char -> String -> [Maybe Char]
updateBoard [] _ _ = []
updateBoard (Just m  : xs) c (k : ks) = Just m : updateBoard xs c ks
updateBoard (Nothing : xs) c (k : ks)
        | c == k    = Just c  : updateBoard xs c ks
        | otherwise = Nothing : updateBoard xs c ks


randomWord :: String -> IO String
randomWord seed = do
        words <- filter valid . lines <$> readFile english
        return $ random words
    where
        valid :: String -> Bool
        valid word = all isAlpha word && length word > 5

        random :: [String] -> String
        random xs = xs !! mod (index * index) (length xs)
        index = sum $ ord <$> seed
        english = "/usr/share/dict/american-english"


game :: Puzzle -> IO ()
game puzzle = do
        clearScreen
        print puzzle

        if winner puzzle
            then putStrLn "Congrats!"
            else if loser puzzle
                    then putStrLn lostMsg
                    else play puzzle <$> getChar >>= game
    where
        clearScreen = callCommand "clear"
        winner (Puzzle s b _) = s == catMaybes b
        loser  (Puzzle _ _ g) = length g == length "hangman"
        lostMsg = "You lose! The word was " ++ secret puzzle


main :: IO ()
main = do
        mapM_ (`hSetBuffering` NoBuffering) [stdout, stdin]

        putStr "Enter a seed word: "
        hSetBuffering stdout LineBuffering
        getLine >>= randomWord >>= game . newPuzzle
