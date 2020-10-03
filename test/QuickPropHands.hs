{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module QuickPropHands (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck ( (==>), Property, Arbitrary(arbitrary)
                       , arbitraryBoundedEnum, applyArbitrary2
                       , counterexample, Gen, discard, forAll, shuffle)
import Data.Maybe (fromJust)
import Control.Lens (makePrisms, has, Prism')
import Data.List (nub, sort)
import Control.Monad (replicateM, unless)
import Debug.Trace (trace)

import Cards
import Hands
import Combinations
import EnumExtra

instance Arbitrary Kind where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Suit where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Card where
    arbitrary = applyArbitrary2 Card

$(makePrisms ''Combination)

tests :: TestTree
tests = testGroup "hands properties"
    [ testProperty "Straight Flush" propStraightFlush
    , testProperty "Low Straight Flush" propLowStraightFlush
    , testProperty "Four of a Kind" propFourOfAKind
    , testProperty "Full House" propFullHouse
    , testProperty "Flush" propFlush
    , testProperty "Straight" propStraight
    , testProperty "Low Straight" propLowStraight
    , testProperty "Three of a Kind" propThreeOfAKind
    , testProperty "Two Pair" propTwoPair
    , testProperty "Pair" propPair
    , testProperty "High Card" propHighCard
    ]

propHighCard :: Property
propHighCard = forAll gen (isCombination _CHighCard)
    where
        gen = do
            cards <- replicateM 5 arbitrary
            let kinds = map cardKind cards
            let suits = map cardSuit cards
            if   (length (nub cards) == 5) -- no duplicate cards
              && (length (nub suits) > 1)  -- no flush
              && (length (nub kinds) == 5) -- no pair, three or four of a kind
              && (notAStraight kinds)
                then shuffle cards
                else discard

propPair :: Property
propPair = forAll gen (isCombination _CPair)
    where
        gen = do
            kind <- arbitrary
            suits <- replicateM 2 arbitrary
            kickers <- replicateM 3 arbitrary
            let cards = map (Card kind) suits ++ kickers
            let suits = map cardSuit cards
            let kinds = map cardKind cards
            if  length (nub cards) == 5 -- no duplicate cards
             && length (nub suits) > 1  -- no flush
             && length (nub kinds) >= 4  -- nothing higher than a pair
                then shuffle cards
                else discard

propTwoPair :: Property
propTwoPair = forAll gen (isCombination _CTwoPair)
    where
        gen = do
            (k1, s11, s12) <- arbitrary
            (k2, s21, s22) <- arbitrary
            kicker <- arbitrary
            let cards = [ Card k1 s11, Card k1 s12
                        , Card k2 s21, Card k2 s22
                        , kicker]
            if  k1 /= k2 -- no three or four of a kind
             && cardKind kicker /= k1 -- no full house
             && cardKind kicker /= k2 -- no full house
             && length (nub cards) == 5 -- no duplicate cards
                then shuffle cards
                else discard

propThreeOfAKind :: Property
propThreeOfAKind = forAll gen (isCombination _CThreeOfAKind)
    where
        gen = do
            kind <- arbitrary
            suit <- arbitrary
            kicker1 <- arbitrary
            kicker2 <- arbitrary
            let trips = map (Card kind) $
                            filter (/= suit) [minBound .. maxBound]
            let cards = kicker1 : kicker2 : trips
            if  length (nub cards) == 5 -- no duplicate cards
             && cardKind kicker1 /= kind -- no four of a kind
             && cardKind kicker2 /= kind -- no four of a kind
             && cardKind kicker1 /= cardKind kicker2 -- no full house
                then shuffle cards
                else discard

propLowStraight :: Property
propLowStraight = forAll gen (isCombination _CStraight)
    where
        gen = do
            suits <- replicateM 5 arbitrary
            let kinds = Ace : [Two .. Five]
            let cards = map (uncurry Card) $ zip kinds suits
            if length (nub suits) > 1 -- no flush
                then shuffle cards
                else discard

propStraight :: Property
propStraight = forAll gen (isCombination _CStraight)
    where
        gen = do
            kind <- arbitrary
            suits <- replicateM 5 arbitrary
            let cards = map (uncurry Card) $
                            zip (enumFromToLeftRel kind 4) suits
            if  length (nub suits) > 1 -- no flush
             && kind >= Six            -- legal straight
                then shuffle cards
                else discard

propFlush :: Property
propFlush = forAll gen (isCombination _CFlush)
    where
        gen = do
            suit <- arbitrary
            kinds <- replicateM 5 arbitrary
            let cards = map (flip Card suit) kinds
            if  length (nub kinds) == 5 -- no duplicate cards
             && notAStraight kinds
                then shuffle cards
                else discard

propFullHouse :: Property
propFullHouse = forAll gen (isCombination _CFullHouse)
    where
        gen = do
            kind1 <- arbitrary
            suits1 <- replicateM 3 arbitrary
            kind2 <- arbitrary
            suits2 <- replicateM 2 arbitrary
            let cards = map (Card kind1) suits1 ++ map (Card kind2) suits2
            if  kind1 /= kind2 -- no four of a kind
             && length (nub cards) == 5 -- no duplicate cards
                then shuffle cards
                else discard

propFourOfAKind :: Property
propFourOfAKind = forAll gen (isCombination _CFourOfAKind)
    where
        gen = do
            kind <- arbitrary
            kicker <- arbitrary
            let cards = kicker : map (Card kind) [minBound .. maxBound]
            if length (nub cards) == 5
                then shuffle cards
                else discard

propStraightFlush :: Property
propStraightFlush = forAll gen (isCombination _CStraightFlush)
    where
        gen = do
            kind <- arbitrary
            suit <- arbitrary
            let cards = map (flip Card suit) $ enumFromToLeftRel kind 4
            if kind >= Six
                then shuffle cards
                else discard

propLowStraightFlush :: Property
propLowStraightFlush = forAll gen (isCombination _CStraightFlush)
    where
        gen = do
            suit <- arbitrary
            let kinds = [Ace, Two, Three, Four, Five]
            let cards = map (flip Card suit) kinds
            shuffle cards

isCombination :: Prism' Combination a -> [Card] -> Property
isCombination combination cards =
    counterexample debug $ has combination combo
    where
        hand = fromJust $ asHand cards
        combo = bestCombination hand
        debug = show hand ++ " => " ++ show combo

notAStraight
    :: [Kind] -- must be 5 cards
    -> Bool
notAStraight kinds = notHighStraight && notLowStraight
    where
        sorted = reverse $ sort kinds
        notHighStraight = sorted /= highStraight
        notLowStraight = sorted /= [Ace, Five, Four, Three, Two]
        highStraight = reverse $ enumFromToLeftRel (head sorted) 4