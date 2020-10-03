module Hands (
      Hand(getHand) -- | not exporting constructor
    , asHand
    , bestCombination
    , isStraightFlush
    , isFourOfAKind
    , isFullHouse
    , isFlush
    , isStraight
    , isThreeOfAKind
    , isTwoPair
    , isPair
    , histogram
) where

import Data.Maybe (fromMaybe)
import Data.Monoid (First(First, getFirst))
import Data.List (sort, sortBy)
import qualified Data.IntMap as IM

import Combinations
import Cards
import EnumExtra

newtype Hand = Hand { getHand :: [Card] }
    deriving Show

asHand :: [Card] -> Maybe Hand
asHand cards = if length cards >= 5 then Just (Hand cards) else Nothing

bestCombination :: Hand -> Combination
bestCombination hand = bestCombo `orElse` highCard
    where
        bestCombo = (getFirst . mconcat . map First) possibleCombos

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

        highCard = case map cardKind (getHand hand) of
            (kind1 : kind2 : kind3 : kind4 : kind5 : _)
                -> CHighCard $ HighCard kind1 kind2 kind3 kind4 kind5

        orElse = flip fromMaybe

isStraightFlush, isFourOfAKind, isFullHouse, isFlush
               , isStraight, isThreeOfAKind, isTwoPair , isPair
               :: Hand -> Maybe Combination

isStraightFlush hand = do
    (CStraight straight) <- isStraight hand
    isFlush hand
    return $ CStraightFlush $ StraightFlush $ straightKind straight

isFourOfAKind (Hand cards) = fmap CFourOfAKind $
    case histogram cards of
        ((kind, 4) : (kicker, _) : _)
            -> Just $ FourOfAKind kind kicker
        _   -> Nothing

isFullHouse (Hand cards) = fmap CFullHouse $
    case histogram cards of
        ((kind1, 3) : (kind2, 2) : _)
            -> Just $ FullHouse kind1 kind2
        _   -> Nothing

isFlush (Hand cards) = fmap CFlush $
    case suits of
        (suit : others) | all (== suit) others
            -> Just $ Flush kind1 kind2 kind3 kind4 kind5
        _   -> Nothing
    where
        suits = map cardSuit cards
        kinds =  map cardKind cards
        (kind1 : kind2 : kind3 : kind4 : kind5 : _) = reverse $ sort kinds

isStraight hand = fmap CStraight $
    case reverse (sort kinds) of
        (Ace : high : rest)
            | high : rest == reverse [Two .. Five]
            -> Just $ Straight high
        (high : rest)
            | high >= Six && high : rest == expected high
            -> Just $ Straight high
        _   -> Nothing
    where
        kinds = map cardKind (getHand hand)
        expected high = reverse $ enumFromToLeftRel high 4

isThreeOfAKind (Hand cards) = fmap CThreeOfAKind $
    case histogram cards of
        ((kind, 3) : (kicker1, 1) : (kicker2, 1) : _)
            -> Just $ ThreeOfAKind kind kicker1 kicker2
        _   -> Nothing

isTwoPair (Hand cards)= fmap CTwoPair $ 
    case histogram cards of
        ((kind1, 2) : (kind2, 2) : (kicker, _) : _)
            -> Just $ TwoPair kind1 kind2 kicker
        _   -> Nothing

isPair (Hand cards) = fmap CPair $
    case histogram cards of
        ((kind, 2) : (kicker1, 1) : (kicker2, 1) : (kicker3, 1) : _)
            -> Just $ Pair kind kicker1 kicker2 kicker3
        _   -> Nothing

histogram :: [Card] -> [(Kind, Int)] -- | Sort by count then by kind
histogram = map toEnum' . reverse . sortBy countThenKind 
                        . IM.toList . (foldr go IM.empty)
    where
        toEnum' (kind, count) = (toEnum kind, count)
        countThenKind (k1, c1) (k2, c2) = compare (c1, k1) (c2, k2)
        go (Card kind _) hist = IM.insertWith (+) (fromEnum kind) 1 hist