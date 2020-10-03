{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module QuickPropHands (
    tests
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck ( (==>), Property, Arbitrary(arbitrary)
                       , arbitraryBoundedEnum, applyArbitrary2
                       , counterexample)
import Data.Maybe (fromJust)
import Control.Lens (makePrisms, has, Prism')
import Data.List (nub, sort)

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

propHighCard
    :: Card
    -> Card
    -> Card
    -> Card
    -> Card
    -> Property
propHighCard c1 c2 c3 c4 c5 =
       length (nub cards) == 5 -- no duplicate cards
    && length (nub suits) > 1  -- no flush
    && length (nub kinds) == 5  -- no pair, three or four of a kind
    && notAStraight kinds
    ==> isCombination _CHighCard cards
    where
        cards = [c1, c2, c3, c4, c5]
        kinds = map cardKind cards
        suits = map cardSuit cards

propPair
    :: Kind
    -> Suit
    -> Suit
    -> Card
    -> Card
    -> Card
    -> Property
propPair k s1 s2 kicker1 kicker2 kicker3 =
       length (nub cards) == 5 -- no duplicate cards
    && length (nub suits) > 1  -- no flush
    && length (nub kinds) >= 4  -- nothing higher than a pair
    ==> isCombination _CPair cards
    where
        kickers = [kicker1, kicker2, kicker3]
        cards = Card k s1 : Card k s2 : kickers
        kinds = k : map cardKind kickers
        suits = s1 : s2 : map cardSuit kickers

propTwoPair
    :: Kind
    -> Suit
    -> Suit
    -> Kind
    -> Suit
    -> Suit
    -> Card
    -> Property
propTwoPair k1 s11 s12 k2 s21 s22 kicker =
       k1 /= k2 -- no three or four of a kind
    && cardKind kicker /= k1 -- no full house
    && cardKind kicker /= k2 -- no full house
    && length (nub cards) == 5 -- no duplicate cards
    ==> isCombination _CTwoPair cards
    where
        cards = [ Card k1 s11, Card k1 s12
                , Card k2 s21, Card k2 s22
                , kicker]

propThreeOfAKind
    :: Kind
    -> Suit
    -> Card
    -> Card
    -> Property
propThreeOfAKind kind suit kicker1 kicker2 =
        length (nub cards) == 5 -- no duplicate cards
    && cardKind kicker1 /= kind -- no four of a kind
    && cardKind kicker2 /= kind -- no four of a kind
    && cardKind kicker1 /= cardKind kicker2 -- no full house
    ==> isCombination _CThreeOfAKind cards
    where
        trips = map (Card kind) $ filter (/= suit) [minBound .. maxBound]
        cards = kicker1 : kicker2 : trips

propLowStraight
    :: Suit
    -> Suit
    -> Suit
    -> Suit
    -> Suit
    -> Property
propLowStraight s1 s2 s3 s4 s5 =
        length (nub suits) > 1 -- no flush
    ==> isCombination _CStraight $
            map (uncurry Card) $ zip kinds suits
    where
        suits = [s1, s2, s3, s4, s5]
        kinds = Ace : [Two .. Five]
    
propStraight
    :: Kind
    -> Suit
    -> Suit
    -> Suit
    -> Suit
    -> Suit
    -> Property
propStraight kind s1 s2 s3 s4 s5 =
        length (nub suits) > 1 -- no flush
    &&  kind >= Six            -- legal straight
    ==> isCombination _CStraight $
            map (uncurry Card) $ zip (enumFromToLeftRel kind 4) suits
    where
        suits = [s1, s2, s3, s4, s5]

propFlush
    :: Suit
    -> Kind
    -> Kind
    -> Kind
    -> Kind
    -> Kind
    -> Property
propFlush suit k1 k2 k3 k4 k5 =
        length (nub kinds) == 5 -- no duplicate cards
    &&  notAStraight kinds
    ==> isCombination _CFlush $
            map (flip Card suit) kinds
    where
        kinds = [k1, k2, k3, k4, k5]

notAStraight
    :: [Kind] -- must be 5 cards
    -> Bool
notAStraight kinds = notHighStraight && notLowStraight
    where
        sorted = reverse $ sort kinds
        notHighStraight = sorted /= highStraight
        notLowStraight = sorted /= [Ace, Five, Four, Three, Two]
        highStraight = reverse $ enumFromToLeftRel (head sorted) 4

propFullHouse
    :: Kind
    -> Suit
    -> Kind
    -> Suit
    -> Suit
    -> Property
propFullHouse k1 s1 k2 s21 s22 =
        s21 /= s22 -- distinct de-selectors
    &&  k1 /= k2   -- no four (or five) of a kind
    ==> isCombination _CFullHouse $
                (map (Card k1) $ filter (/= s1) [minBound .. maxBound])
            ++  (map (Card k2) $ filter (/= s21)
                               $ filter (/= s22) [minBound .. maxBound])

propFourOfAKind
    :: Kind
    -> Card
    -> Property
propFourOfAKind kind kicker =
        cardKind kicker /= kind
    ==> isCombination _CFourOfAKind $
            kicker : map (Card kind) [minBound .. maxBound]

propStraightFlush
    :: Kind
    -> Suit
    -> Property
propStraightFlush kind suit =
        kind >= Six
    ==> isCombination _CStraightFlush $
            map (flip Card suit) $ enumFromToLeftRel kind 4

isCombination :: Prism' Combination a -> [Card] -> Property
isCombination combination cards =
    counterexample debug $ has combination combo
    where
        hand = fromJust $ asHand cards
        combo = bestCombination hand
        debug = show hand ++ " => " ++ show combo