Title: Hangman in Haskell
Date: 2018-10-28
Category: Programming
Tags: programming, games, haskell
Status: published
Summary: Implementing a simple game in functional style

# Introduction

Hangman is a simple word game where the user must guess the hidden word. The
catch is that there are a limited number of guesses. This is a functional
implementation of Hangman in 100 lines. I'll go through each section of the
source and explain what's going on.

```
         H a n g m a

Board:   h o m e b o d y

Guessed: a g i r s t

Congrats!
```

You can find the complete runnable source
[here](https://public.anardil.net/share/code/source/hangman.hs).

# Setup

We're going to be working with text, so `Data.Char` and `Data.List` will be
useful. The `System` imports will let us control user input and our output to
the screen. `Data.Maybe` will help us with the 'board' part of our Puzzle.
These will make more sense as we see them in context.

```haskell
module Main where

import           Data.Char      (isAlpha, ord, toLower)
import           Data.List      (intersperse, sort)
import           Data.Maybe     (catMaybes, fromMaybe)
import           System.IO      (BufferMode (..), hSetBuffering, stdin, stdout)
import           System.Process (callCommand)
```

# Types

We need a structure to contain the state of the world while we play the
game. There are three things important to us:

- `secret` the secret word

- `board` the list of letters that we've guessed correctly, but also a list of
  blanks for the letters we haven't guessed yet

- `guessed` the letters we've guessed, but aren't in the secret word

In Haskell, one way to represent this is:

```haskell
data Puzzle = Puzzle
            { secret  :: String
            , board   :: [Maybe Char]
            , guessed :: String
            }
```

The 'Puzzle' type packages up these elements and is what we'll thread through
the game to maintain state. Looking ahead, we can imagine that the core part of
our game will be something to the effect of:

`game :: Puzzle -> UserInput -> Puzzle`

We'll ask for the user input, look at the current state of the world, and build
a new state of the world from the input.

Defining a way to 'string-ify' our puzzle gives us an easy way to display the
state of the game as we run. Pattern matching lets us extract the components of
our Puzzle for use in the rest of the function.

- The title doubles as an indicator of how many incorrect guesses the player
  can make. Each incorrect guess will uncover more of `H A N G M A N`.

- The board is a series of letters or blanks. `Nothing` is a handy way to
  represent that the board is empty in that position. `fromMaybe '_' <$> w`
  says: for each character, if it's Nothing return '_', otherwise return the
  letter.

- The guessed line is straight forward, it's just a sorted list of incorrect
  letters.

```haskell
instance Show Puzzle where

    show (Puzzle s b g) =
            concat
            [ "\n"
            , "         ", space showTitle, "\n"
            , "\n"
            , "Board:   ", space showBoard, "\n"
            , "\n"
            , "Guessed: ", space showGuessed
            , "\n"
            ]

        where showBoard = fromMaybe '_' <$> b
              showTitle = take (length g) "Hangman"
              showGuessed = sort g
              space = intersperse ' '
```

# Working with the data structures

Now that we have our game state, we need ways to build and mutate it.
`newPuzzle` takes a secret word and builds a Puzzle around it. The board begins
as a list of `Nothing` the same length as the secret word.  It'll be filled in
by user input. The guessed list starts empty.

```haskell
newPuzzle :: String -> Puzzle
newPuzzle s = Puzzle secret board guessed
    where
        board   = const Nothing <$> s
        secret  = toLower <$> s
        guessed = []
```

`play` is the actual implementation of the game we guessed earlier while
defining `Puzzle`. It takes previous game state and user input, and returns a
new game state. There are essentially three cases to consider:

- The input character is already on the board or in guessed, do nothing

- The input character is in the secret word, update the board

- The input character is not in the secret word, add it to the guessed letters

```haskell
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
```

The tricky part is when the player is correct. We need to find the correct
position in the board (a list of `Maybe Char`) and fill it in, without mutating
the other characters - blank or not. `updateBoard` takes the current board, the
player input, the secret word, and returns a new board.

We need to step through the board and secret word together. We could use a
combination of `zip` and `map` here, but explicit recursion and pattern
matching feels more straight forward. When we encounter a blank (`Nothing`), if
the current letter in the secret word is the player's input, we add it to the
board. Otherwise, keep `Nothing` in that position.

```haskell
updateBoard :: [Maybe Char] -> Char -> String -> [Maybe Char]
updateBoard [] _ _ = []
updateBoard (Just m  : xs) c (k : ks) = Just m : updateBoard xs c ks
updateBoard (Nothing : xs) c (k : ks)
        | c == k    = Just c  : updateBoard xs c ks
        | otherwise = Nothing : updateBoard xs c ks
```

# Getting a secret word

We won't have much of a game if we ask the player to give us the secret word.
Instead, we'll pull a random word out of the system dictionary (assuming
a Debian-like installation). The word won't be completely random, but pseudo
random based on a 'seed word' we ask the user.

First, we'll read in the words from disk and filter out invalid words. The
system dictionary contains words with apostrophes and very short words (which
are very difficult to guess), we don't want to include those.

To get a 'random' word from our list, we'll convert the seed word to a number,
mutate it a bit, and use it as an index into the list.

```haskell
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
```

# Playing the game, or IO

Time to pull it all together and run the main game loop. `main` sets up
buffering for stdin and stdout (allows us to skip the enter key while entering
letters), builds the random word and starts the game.

`game` is the main loop, and handles the game logic:

- did we win?

- did we lose?

- play a turn and continue the game

To keep things pretty and easy to read, we call `clear` before printing the
game state each turn. This could be done with `System.Console.Terminfo`, but
that's quite a can of worms and unnecessary here.

By placing the `winner` check before the `loser` check, we allow the player to
win the game after using all of their guesses.

```haskell
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
```

# Summary

We built hangman by defining the game state, a couple functions for mutating
it, and the wrappers to talk with the outside world. Haskell let us focus on
the logic of our game instead of low level details, since the type system and
pattern matching do the heavy lifting for us.
