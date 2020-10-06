module Showdown (
      stats
    , showdown
    , evalGame
    , Game(..)
    , FinishedGame(..)
    , Stats(..)
    , Outcome(..)
)
where

import qualified Data.Set as S
import Data.Maybe (fromJust, isJust)
import Data.List.Split (chunksOf)
import Combinatorics (tuples)

import Cards
import Hands

newtype Pocket = Pocket {getPocket :: [Card]}
    deriving Show

newtype Community = Community {getCommunity :: [Card]}
    deriving Show

data Outcome = Win | Loose | Split
    deriving (Eq, Show)

instance Semigroup Outcome where
    Loose <> _     = Loose
    _     <> Loose = Loose
    Split <> Win   = Split
    Win   <> Split = Split
    _     <> _     = Win

instance Monoid Outcome where
    mempty = Win

data Stats = Stats
    { statsWin :: Int
    , statsLoose :: Int
    , statsSplit :: Int
    }
    deriving Show

{- | A Game with some cards on the table.

properties:
 * length gamePockets `elem` [0 .. gamePlayers]
 * all (map ((`elem` [0,1]) . length) gamePockets)
 * length gameCommunity `elem` [0..5]
-}
data Game = Game
    { gamePlayers :: Int
    , gamePockets :: [[Card]]
    , gameCommunity :: [Card]
    }
    deriving (Show, Eq)

{- | A Game with all cards on the table

That is two pocket cards per player and 5 community cards
-}
newtype FinishedGame = FinishedGame {getGame :: Game}
    deriving (Eq)

stats :: [Outcome] -> Stats
stats results =
    Stats (percent win) (percent loose) (percent split)
    where
        (total, win, loose, split) =
            foldr classify (0, 0, 0, 0) results

        classify Win (t, w, l, s) = (t + 1, w + 1, l, s)
        classify Loose (t, w, l, s) = (t + 1, w, l + 1, s)
        classify Split (t, w, l, s) = (t + 1, w, l, s + 1)

        percent x = (x * 100) `div` total

{- | Flatten a game into a list of dealt and undealt cards.

The order is in sync with `construct`, but carries no semantics.

Example with one other player and a flop on the table.

>>> :{
    deconstruct (Game 2
        [[Card Ace Spades, Card Ace Clubs]]
        [Card King Spades, Card King Clubs, Card King Diamonds])
 ==
    [ Just (Card Ace Spades), Just (Card Ace Clubs)
    , Nothing, Nothing
    , Just (Card King Spades), Just (Card King Clubs)
    , Just (Card King Diamonds), Nothing, Nothing]
:}
True
-}
deconstruct :: Game -> [Maybe Card]
deconstruct (Game n ps cs) = pockets ++ community
    where
        community = map Just cs ++ replicate (5 - length cs) Nothing
        allPockets = concat ps
        pockets = map Just allPockets
               ++ replicate (2 * n - length allPockets) Nothing

{- | Deal missing cards.

>>> :{
    deal [Just (Card Ace Spades), Nothing] [Card Ace Clubs]
==
    [Card Ace Spades, Card Ace Clubs]
:}
True
-}
deal :: [Maybe Card] -> [Card] -> [Card]
deal [] [] = []
deal (Nothing : xs) (y : ys) = y : deal xs ys
deal (Just x : xs) ys        = x : deal xs ys
deal _ _ = error "mismatch in number of cards"

{- | Reconstruct a Game for a number of players from a list of completely
     dealt cards.

>>> :{
    construct 2 [ Card Ace Spades, Card Ace Clubs
        , Card King Spades, Card King Clubs
        , Card Queen Spades, Card Queen Clubs, Card Jack Diamonds
        , Card Jack Spades, Card Jack Hearts]
 ==
    FinishedGame (Game 2
       [ [Card Ace Spades, Card Ace Clubs]
       , [Card King Spades, Card King Clubs]
       ]
       [ Card Queen Spades, Card Queen Clubs, Card Jack Diamonds
       , Card Jack Spades, Card Jack Hearts
       ])
:}
True
-}
construct :: Int -> [Card] -> FinishedGame
construct n cards
    | length cards == 2 * n + 5 = FinishedGame $
        Game n (chunksOf 2 pockets) community
    | otherwise = error "wrong number of cards"
    where
        (pockets, community) = splitAt (2 * n) cards

{- | Evaluate a FinishedGame from the perspective of the first player.

>>> :{
    evalGame (FinishedGame (Game 2
        [ [Card Ace Spades, Card Ace Clubs]
        , [Card King Spades, Card King Clubs]
        ]
        [ Card Two Diamonds, Card Three Diamonds, Card Four Diamonds
        , Card Two Hearts, Card Five Hearts
        ]))
:}
Win
-}
evalGame :: FinishedGame -> Outcome
evalGame (FinishedGame (Game n ps cs))
    | n == 0 = Split
    | n == 1 = Win
    | otherwise = foldMap against theirPockets
    where
        (myPocket : theirPockets) = ps
        myHand = fromJust $ asHand $ myPocket ++ cs

        against theirPocket =
            let theirHand = fromJust $ asHand $ theirPocket ++ cs in
            outcome myHand theirHand

{- | Determine the game Outcome between the player and one opponent.

>>> :{
    outcome
        (fromJust (asHand
            [ Card Ace Spades, Card Ace Clubs
            , Card Ace Hearts, Card Ace Diamonds
            , Card Two Clubs ]))
        (fromJust (asHand
            [ Card King Spades, Card King Clubs
            , Card King Hearts, Card King Diamonds
            , Card Three Clubs ]))
:}
Win
-}
outcome :: Hand -> Hand -> Outcome
outcome myHand theirHand =
    case compare myCombo theirCombo of
        LT -> Loose
        EQ -> Split
        GT -> Win
    where
        myCombo = bestCombination myHand
        theirCombo = bestCombination theirHand

{- | Explore all possible ways a game could end.

-}
showdown :: Game -> [FinishedGame]
showdown game = possibleGames
    where
        withMissingCards = deconstruct game
        dealtCards = map fromJust $ filter isJust withMissingCards
        numberofMissingCards = length withMissingCards - length dealtCards

        options = tuples numberofMissingCards (remainingDeck dealtCards)
        possibleGames = map (construct (gamePlayers game)
                            . deal withMissingCards) options

{- | All cards of a deck.

>>> S.size fullDeck
52
-}
fullDeck :: S.Set Card
fullDeck = S.fromList [minBound .. maxBound]

{- | Remainin deck after a few cards have been dealt.

>>> length (remainingDeck [Card Ace Spades, Card Ace Clubs])
50
-}
remainingDeck :: [Card] -> [Card]
remainingDeck dealt = S.toDescList (fullDeck `S.difference` S.fromList dealt)