module Main where

import Control.Monad (forM_)

import Cards
import Showdown

games :: [Game]
games = [
    Game 2 [[Card Ace Spades, Card Ace Clubs]]
    [ Card Two Hearts, Card Three Hearts, Card King Spades
    , Card Queen Diamonds, Card Seven Clubs
    ]
    ]

printGameStats :: IO ()
printGameStats = forM_ games $ \game -> do
    let gameStats = stats (map evalGame (showdown game))
    print game
    print gameStats
    print "---"

printNonWinningGames :: IO ()
printNonWinningGames = forM_ games $ \game -> do
    print "----"
    print game
    forM_ (showdown game) $ \finishedGame -> do
        let theirPocket = show (gamePockets (getGame finishedGame) !! 1)
        case evalGame finishedGame of
            Loose -> print $ "Loose: " ++ theirPocket
            Split -> print $ "Split: " ++ theirPocket
            Win   -> pure ()
    print "---"

main :: IO ()
main = printNonWinningGames
