{-# LANGUAGE TupleSections #-}
module Showdown where

import qualified Data.Set as S
import Data.Maybe (fromJust)
import Combinatorics (tuples)

import Cards
import Hands

newtype Pocket = Pocket {getPocket :: [Card]}
    deriving Show

newtype Community = Community {getCommunity :: [Card]}
    deriving Show

data Status = Win | Loose | Split
    deriving (Eq, Show)

data Stats = Stats
    { statsWin :: Int
    , statsLoose :: Int
    , statsSplit :: Int
    }
    deriving Show

stats :: [(Status, Pocket)] -> Stats
stats results =
    Stats (percent win) (percent loose) (percent split)
    where
        (total, win, loose, split) = foldr classify (0, 0, 0, 0) $ map fst results

        classify Win (t, w, l, s) = (t + 1, w + 1, l, s)
        classify Loose (t, w, l, s) = (t + 1, w, l + 1, s)
        classify Split (t, w, l, s) = (t + 1, w, l, s + 1)

        percent x = (x * 100) `div` total

showdown :: Pocket -> Community -> [(Status, Pocket)]
showdown myPocket community =
    map (testOption . Pocket) (tuples 2 (deck myCards))
    where
        myCards = getPocket myPocket ++ getCommunity community

        myCombo = bestCombination (fromJust (asHand myCards))

        testOption theirPocket = case compare myCombo theirCombo of
            LT -> res Loose
            EQ -> res Split
            GT -> res Win
            where
                theirCards = getPocket theirPocket ++ getCommunity community
                theirCombo = bestCombination (fromJust (asHand theirCards))
                res = (, theirPocket)

fullDeck :: S.Set Card
fullDeck = S.fromList [minBound .. maxBound]

deck :: [Card] -> [Card]
deck dealt = S.toDescList (fullDeck `S.difference` (S.fromList dealt))