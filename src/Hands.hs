module Hands where

import Data.Maybe (fromMaybe)
import Data.Monoid (First(First, getFirst))
import Data.List (sort)

import Combinations
import Cards


bestCombination :: [Card] -> Combination
bestCombination hand = bestCombo `orElse` highCard
    where
        bestCombo = getFirst $ mconcat $ map First $ possibleCombos
        possibleCombos = 
            [ isStraightFlush hand
            , isFourOfAKind hand
            , isFullHouse hand
            , isFlush hand
            , isStraight hand
            , isThreeOfAKind hand
            , isTwoPair hand
            , isPair hand
            ]
        highCard = CHighCard $ HighCard $ orderDesc $ map cardKind hand
        orElse = flip fromMaybe

isStraightFlush, isFourOfAKind, isFullHouse, isFlush
               , isStraight, isThreeOfAKind, isTwoPair , isPair
               :: [Card] -> Maybe Combination

isStraightFlush hand = do
    (CStraight straight) <- isStraight hand
    isFlush hand
    return $ CStraightFlush $ StraightFlush $ straightKind straight

isFourOfAKind hand = Nothing
isFullHouse hand = Nothing

isFlush hand = fmap CFlush $
    case suits of
        (suit : others) | all (== suit) others
            -> Just $ Flush ordered
        _   -> Nothing
    where
        suits = map cardSuit hand
        kinds =  map cardKind hand
        ordered = orderDesc kinds

isStraight hand = fmap CStraight $
    case orderedRanks of
        (high : _ )
            | high >= 5 && orderedRanks == expectedRanks high
            -> Just $ Straight (toEnum high)
        _   -> Nothing
    where
        kinds = map cardKind hand
        orderedRanks = map fromEnum (reverse $ sort kinds)
        expectedRanks high = reverse [high-4 .. high]


isThreeOfAKind hand = Nothing
isTwoPair hand = Nothing
isPair hand = Nothing