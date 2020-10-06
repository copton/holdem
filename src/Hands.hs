{-# LANGUAGE ScopedTypeVariables #-}
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
) where

import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Monoid (First(First, getFirst))
import Data.List (sortOn, sortBy, isPrefixOf, tails)
import qualified Data.IntMap as IM
import Data.Ord (Down(Down))

import Combinations
import Cards

newtype Hand = Hand { getHand :: [Card] }
    deriving Show

-- | Smart constructor for hands
--
-- Enforces that at least 5 cards are in the hand.
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
            _ -> error "hand will less than 5 cards"

        orElse = flip fromMaybe

isStraightFlush, isFourOfAKind, isFullHouse, isFlush
               , isStraight, isThreeOfAKind, isTwoPair , isPair
               :: Hand -> Maybe Combination

{- | Test if a hand is a `StraightFlush`

Example with 5 cards

>>> :{
    isStraightFlush (Hand [ Card King Clubs, Card Nine Clubs, Card Jack Clubs
                          , Card Ten Clubs, Card Queen Clubs])
 ==
    Just (CStraightFlush (StraightFlush King))
:}
True

Example with 5 cards and low Ace

>>> :{
    isStraightFlush (Hand [ Card Ace Hearts, Card Two Hearts, Card Four Hearts
                          , Card Five Hearts, Card Three Hearts])
 ==
    Just (CStraightFlush (StraightFlush Five))
:}
True

Example with 7 cards

>>> :{
    isStraightFlush (Hand [ Card King Clubs, Card Nine Clubs, Card Jack Clubs
                          , Card King Diamonds, Card Two Clubs
                          , Card Ten Clubs, Card Queen Clubs])
 ==
    Just (CStraightFlush (StraightFlush King))
:}
True

Negative example with 5 cards

>>> :{
    isStraightFlush (Hand [ Card King Hearts, Card Nine Clubs, Card Jack Clubs
                          , Card Ten Diamonds, Card Queen Hearts])
:}
Nothing
-}
isStraightFlush hand = fmap CStraightFlush $
    getFirst $ mconcat $ map isStraightWith [minBound .. maxBound]
    where
        isStraightWith :: Suit -> First StraightFlush
        isStraightWith suit = First $ do
            hand' <- asHand $ filter ((==suit) . cardSuit) $ getHand hand
            combo <- isStraight hand'
            return $ promote combo

        promote (CStraight (Straight kind)) = StraightFlush kind
        promote _ = error "can only promote straights"

{- | Test if hand is a `FourOfAKind`

Example with 5 cards

>>> :{
    isFourOfAKind (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                        , Card King Diamonds, Card King Spades])
 ==
    Just (CFourOfAKind (FourOfAKind King Jack))
:}
True

Example with 7 cards

>>> :{
    isFourOfAKind (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                        , Card Queen Diamonds, Card Two Hearts
                        , Card King Diamonds, Card King Spades])
 ==
    Just (CFourOfAKind (FourOfAKind King Queen))
:}
True

Negative example with 5 cards

>>> :{
    isFourOfAKind (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                        , Card Queen Diamonds, Card King Spades])
:}
Nothing
-}
isFourOfAKind (Hand cards) = fmap CFourOfAKind $
    case kindHistogram cards of
        ((kind, 4) : (kicker, _) : _)
            -> Just $ FourOfAKind kind kicker
        _   -> Nothing

{- | Test if a hand is a `FullHouse`

Example with 5 cards:

>>> :{
    isFullHouse (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                      , Card King Diamonds, Card Jack Hearts ])
 ==
    Just (CFullHouse (FullHouse King Jack))
:}
True

Example with 7 cards:

>>> :{
    isFullHouse (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                      , Card Seven Diamonds, Card Five Spades
                      , Card King Diamonds, Card Jack Hearts ])
 ==
    Just (CFullHouse (FullHouse King Jack))
:}
True
-}
isFullHouse (Hand cards) = fmap CFullHouse $
    case kindHistogram cards of
        ((kind1, 3) : (kind2, 2) : _)
            -> Just $ FullHouse kind1 kind2
        _   -> Nothing

{- | Test if a hand is a `Flush`

Example with 5 cards

>>> :{
    isFlush (Hand [ Card King Clubs, Card Ten Clubs, Card Eight Clubs
                  , Card Ace Clubs, Card Two Clubs ])
 ==
    Just (CFlush (Flush Ace King Ten Eight Two))
:}
True

Negative example with 5 cards

>>> :{
 isFlush $ Hand [ Card King Hearts, Card Ten Clubs, Card Eight Clubs
                , Card Ace Clubs, Card Two Clubs ]
:}
Nothing

Example with 7 cards

>>> :{
    isFlush (Hand [ Card King Hearts, Card Ten Clubs, Card Eight Clubs
                  , Card Ace Clubs, Card Two Clubs, Card Seven Hearts
                  , Card Nine Clubs ])
 ==
    Just (CFlush (Flush Ace Ten Nine Eight Two))
:}
True
-}
isFlush (Hand cards) = fmap CFlush $
    case suitHistogram cards of
        ((suit, n) : _) | n >= 5 ->
            let flushCards = filter ((==suit) . cardSuit) cards
                kinds = sortOn Down $ map cardKind flushCards
                (kind1 : kind2 : kind3 : kind4 : kind5 : _) = kinds
            in Just $ Flush kind1 kind2 kind3 kind4 kind5
        _ -> Nothing

{- | Test if a hand is a `Straight`

Example with 5 cards

>>> :{
    isStraight (Hand [ Card King Hearts, Card Nine Clubs, Card Jack Clubs
                     , Card Ten Diamonds, Card Queen Hearts])
 ==
     Just (CStraight (Straight King))
:}
True

Example with 7 cards

>>> :{
    isStraight (Hand [ Card King Hearts, Card Nine Clubs, Card Jack Clubs
                     , Card Queen Hearts, Card King Clubs
                     , Card Ten Diamonds, Card Queen Hearts])
 ==
     Just (CStraight (Straight King))
:}
True

Example with 5 cards and low `Ace`

>>> :{
    isStraight (Hand [ Card Ace Hearts, Card Three Clubs, Card Two Clubs
                     , Card Five Diamonds, Card Four Hearts])
 ==
     Just (CStraight (Straight Five))
:}
True

Example with 7 cards and low `Ace`

>>> :{
    isStraight (Hand [ Card Ace Hearts, Card Three Clubs, Card Two Clubs
                     , Card Queen Hearts, Card King Clubs
                     , Card Five Diamonds, Card Four Hearts])
 ==
     Just (CStraight (Straight Five))
:}
True

Negative Example with 5 cards

>>> :{
    isStraight (Hand [ Card Ace Hearts, Card Nine Clubs, Card Jack Clubs
                     , Card Ten Diamonds, Card Queen Hearts])
:}
Nothing
-}
isStraight hand = fmap CStraight $
    case findInfix [1,1,1,1] gps of
        Just i -> Just $ Straight $ kinds !! i
        Nothing | Set.member Ace kindsSet ->
            case findInfix [1,1,1] gps of
                Just i | kinds !! i == Five -> Just $ Straight Five
                _ -> Nothing
        _ -> Nothing
    where
        kindsSet = Set.fromList $ map cardKind $ getHand hand
        kinds = Set.toDescList kindsSet
        gps = gaps kinds

{- | Test if hand is `ThreeOfAKind`

Example with 5 cards

>>> :{
    isThreeOfAKind (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                         , Card King Diamonds, Card Queen Hearts])
 ==
    Just (CThreeOfAKind (ThreeOfAKind King Queen Jack))
:}
True

Example with 7 cards

>>> :{
    isThreeOfAKind (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                         , Card Two Clubs, Card Four Diamonds
                         , Card King Diamonds, Card Queen Hearts])
 ==
    Just (CThreeOfAKind (ThreeOfAKind King Queen Jack))
:}
True

FullHouse is not Three of a Kind

>>> :{
    isThreeOfAKind (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                         , Card King Diamonds, Card Jack Hearts])
:}
Nothing

For of a Kind is not Three of a Kind

>>> :{
    isThreeOfAKind (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                         , Card King Diamonds, Card King Spades])
:}
Nothing
-}
isThreeOfAKind (Hand cards) = fmap CThreeOfAKind $
    case kindHistogram cards of
        ((kind, 3) : (kicker1, 1) : (kicker2, 1) : _)
            -> Just $ ThreeOfAKind kind kicker1 kicker2
        _   -> Nothing

{- | Test if a hand is a `TwoPair`

Example with 5 cards

>>> :{
    isTwoPair (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                    , Card Jack Diamonds, Card Queen Hearts])
 ==
    Just (CTwoPair (TwoPair King Jack Queen))
:}
True

Example with 7 cards

>>> :{
    isTwoPair (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                    , Card Two Hearts, Card Three Diamonds
                    , Card Jack Diamonds, Card Queen Hearts])
 ==
    Just (CTwoPair (TwoPair King Jack Queen))
:}
True

Negative example with 5 cards

>>> :{
    isTwoPair (Hand [ Card Two Hearts, Card King Clubs, Card Jack Clubs
                    , Card Three Diamonds, Card Queen Hearts])
:}
Nothing
-}
isTwoPair (Hand cards)= fmap CTwoPair $
    case kindHistogram cards of
        ((kind1, 2) : (kind2, 2) : (kicker, _) : _)
            -> Just $ TwoPair kind1 kind2 kicker
        _   -> Nothing

{- | Test if hand is a `Pair`

Example with 5 cards

>>> :{
    isPair (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                 , Card Ten Diamonds, Card Queen Hearts])
 ==
      Just (CPair (Pair King Queen Jack Ten))
:}
True

Two Pair is not Pair

>>> :{
    isPair (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                 , Card Jack Diamonds, Card Queen Hearts])
:}
Nothing

Three of a Kind is not Pair

>>> :{
    isPair (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                 , Card King Diamonds, Card Queen Hearts])
:}
Nothing

FullHouse is not Pair

>>> :{
    isPair (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                 , Card King Diamonds, Card Jack Hearts])
:}
Nothing

For of a Kind is not Pair

>>> :{
    isPair (Hand [ Card King Hearts, Card King Clubs, Card Jack Clubs
                 , Card King Diamonds, Card King Spades])
:}
Nothing
-}
isPair (Hand cards) = fmap CPair $
    case kindHistogram cards of
        ((kind, 2) : (kicker1, 1) : (kicker2, 1) : (kicker3, 1) : _)
            -> Just $ Pair kind kicker1 kicker2 kicker3
        _   -> Nothing


{- | Histogram over the kinds of a list of cards

Returns the list of kinds and their occurence,
sorted by occurence then by kind.

>>> kindHistogram []
[]

>>> kindHistogram [Card Jack Hearts]
[(Jack,1)]

>>> kindHistogram [Card Jack Hearts, Card Jack Spades]
[(Jack,2)]

>>> kindHistogram [Card King Hearts, Card King Clubs, Card Jack Clubs]
[(King,2),(Jack,1)]
-}
kindHistogram :: [Card] -> [(Kind, Int)]
kindHistogram = map toEnum' . sortBy countThenKindReverse
                        . IM.toList . foldr go IM.empty
    where
        toEnum' (kind, count) = (toEnum kind, count)
        countThenKindReverse (k1, c1) (k2, c2) = compare (c2, k2) (c1, k1)
        go (Card kind _) = IM.insertWith (+) (fromEnum kind) 1

{- | Histogram over the suits of a list of cards

Returns the list of suits and their occurence,
sorted by occurence then by suit.

>>> suitHistogram []
[]

>>> suitHistogram [Card Jack Hearts]
[(Hearts,1)]

>>> suitHistogram [Card Jack Hearts, Card King Hearts]
[(Hearts,2)]

>>> suitHistogram [Card King Hearts, Card King Clubs, Card Jack Clubs]
[(Clubs,2),(Hearts,1)]
-}
suitHistogram :: [Card] -> [(Suit, Int)]
suitHistogram = map toEnum' . sortBy countThenSuitReverse
                       . IM.toList . foldr go IM.empty
    where
        toEnum' (suit, count) = (toEnum suit, count)
        countThenSuitReverse (s1, c1) (s2, c2) = compare (c2, s2) (c1, s1)
        go (Card _ suit) = IM.insertWith (+) (fromEnum suit) 1

{- | Analyse the gaps between a list of descendingly sorted Kinds

>>> gaps [Ace, King]
[1]

>>> gaps [Ten, Seven, Five]
[3,2]

>>> gaps [Five, Five, Three]
[0,2]
-}
gaps :: [Kind] -> [Int]
gaps [] = []
gaps [_] = []
gaps cards = zipWith (-) cards' (tail cards')
    where
        cards' = map fromEnum cards

{- | Find the first item

>>> findInfix [1] [0,1,2]
Just 1

>>> findInfix [1] [2,3,4]
Nothing

>>> findInfix [] [1,2,3]
Just 0

>>> findInfix [1] [0,1,2,0,1,2]
Just 1

-}
findInfix :: forall a. Eq a => [a] -> [a] -> Maybe Int
findInfix needle haystack = getFirst $ mconcat $ map isPrefix options
    where
        options :: [(Int, [a])]
        options = zip [0..] (tails haystack)

        isPrefix :: (Int, [a]) -> First Int
        isPrefix (idx, haystack')
            | needle `isPrefixOf` haystack' = First (Just idx)
            | otherwise                     = First Nothing