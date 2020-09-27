{-# LANGUAGE DeriveFunctor #-}
module Combinations (
    Combination(..)
  , OrderedDesc -- | not exporting constructor
  , orderDesc
  , HighCard(..)
  , Pair(..)
  , TwoPair(..)
  , ThreeOfAKind(..)
  , Straight(..)
  , Flush(..)
  , FullHouse(..)
  , FourOfAKind(..)
  , StraightFlush(..)
) where

import Cards
import Data.List (sort)

data Combination
    = CHighCard HighCard
    | CPair Pair
    | CTwoPair TwoPair
    | CThreeOfAKind ThreeOfAKind
    | CStraight Straight
    | CFlush Flush
    | CFullHouse FullHouse
    | CFourOfAKind FourOfAKind
    | CStraightFlush StraightFlush
    deriving (Show, Eq)

rank :: Combination -> Int
rank c = case c of
    CHighCard      _ -> 0
    CPair          _ -> 1
    CTwoPair       _ -> 2
    CThreeOfAKind  _ -> 3
    CStraight      _ -> 4
    CFlush         _ -> 5
    CFullHouse     _ -> 6
    CFourOfAKind   _ -> 7
    CStraightFlush _ -> 8

newtype OrderedDesc a = OrderedDesc { unOrderedDesc :: [a] }
    deriving (Show, Eq, Ord, Functor)

orderDesc :: Ord a => [a] -> OrderedDesc a
orderDesc = OrderedDesc . reverse . sort

data HighCard = HighCard
    { highCardOrdered :: OrderedDesc Kind
    }
    deriving (Show, Eq, Ord)

data Pair = Pair
    { pairKind     :: Kind
    , pairKicker   :: OrderedDesc Kind
    }
    deriving (Show, Eq, Ord)

data TwoPair = TwoPair
    { twoPairHighPair :: Kind
    , twoPairLowPair  :: Kind
    , twoPairKicker   :: OrderedDesc Kind
    }
    deriving (Show, Eq, Ord)

data ThreeOfAKind = ThreeOfAKind
    { threeOfAKindKind   :: Kind
    , threeOfAKindKicker :: OrderedDesc Kind
    }
    deriving (Show, Eq, Ord)

data Straight = Straight
    { straightKind :: Kind -- | hightest card
    }
    deriving (Show, Eq, Ord)

data Flush = Flush
    { flushOrdered :: OrderedDesc Kind
    }
    deriving (Show, Eq, Ord)

data FullHouse = FullHouse
    { fullHouseThreeOfAKind :: Kind
    , fullHousePair         :: Kind
    }
    deriving (Show, Eq, Ord)

data FourOfAKind = FourOfAKind
    { fourOfAKindKind :: Kind
    }
    deriving (Show, Eq, Ord)

data StraightFlush = StraightFlush
    { straightFlushKind :: Kind -- | highest card
    }
    deriving (Show, Eq, Ord)

instance Ord Combination where
    compare (CHighCard left     ) (CHighCard right     ) = compare left right
    compare (CPair left         ) (CPair right         ) = compare left right
    compare (CTwoPair left      ) (CTwoPair right      ) = compare left right
    compare (CThreeOfAKind left ) (CThreeOfAKind right ) = compare left right
    compare (CStraight left     ) (CStraight right     ) = compare left right
    compare (CFlush left        ) (CFlush right        ) = compare left right
    compare (CFullHouse left    ) (CFullHouse right    ) = compare left right
    compare (CFourOfAKind left  ) (CFourOfAKind right  ) = compare left right
    compare (CStraightFlush left) (CStraightFlush right) = compare left right
    compare left right = compare (rank left) (rank right)