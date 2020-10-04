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
import Data.List (sort, sortBy, isPrefixOf, tails)
import qualified Data.IntMap as IM

import Combinations
import Cards
import EnumExtra

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
                kinds = reverse $ sort $ map cardKind flushCards
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

{-
    let
    case reverse (sort kinds) of
        (Ace : high : rest)
            | (high : rest) `isPrefixOf` (reverse [Two .. Five])
            -> Just $ Straight high
        (high : rest)
            | high >= Six && (high : rest) `isPrefixOf ` (expected high)
            -> Just $ Straight high
        _   -> Nothing
    where
        kinds = reverse $ sort $ map cardKind (getHand hand)
        gps = gaps kinds
        highIndex = findInfix [1, 1, 1, 1] gps
        -}

isThreeOfAKind (Hand cards) = fmap CThreeOfAKind $
    case kindHistogram cards of
        ((kind, 3) : (kicker1, 1) : (kicker2, 1) : _)
            -> Just $ ThreeOfAKind kind kicker1 kicker2
        _   -> Nothing

isTwoPair (Hand cards)= fmap CTwoPair $
    case kindHistogram cards of
        ((kind1, 2) : (kind2, 2) : (kicker, _) : _)
            -> Just $ TwoPair kind1 kind2 kicker
        _   -> Nothing

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
kindHistogram = map toEnum' . reverse . sortBy countThenKind
                        . IM.toList . (foldr go IM.empty)
    where
        toEnum' (kind, count) = (toEnum kind, count)
        countThenKind (k1, c1) (k2, c2) = compare (c1, k1) (c2, k2)
        go (Card kind _) hist = IM.insertWith (+) (fromEnum kind) 1 hist

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
suitHistogram = map toEnum' . reverse . sortBy countThenSuit
                       . IM.toList . (foldr go IM.empty)
    where
        toEnum' (suit, count) = (toEnum suit, count)
        countThenSuit (s1, c1) (s2, c2) = compare (c1, s1) (c2, s2)
        go (Card _ suit) hist = IM.insertWith (+) (fromEnum suit) 1 hist

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