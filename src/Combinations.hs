module Combinations (
    Combination(..)
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

data HighCard = HighCard -- ordered by rank of kind
    { highCard1 :: Kind
    , highCard2 :: Kind
    , highCard3 :: Kind
    , highCard4 :: Kind
    , highCard5 :: Kind
    }
    deriving (Show, Eq, Ord)

data Pair = Pair -- ordered by rank of kind
    { pairKind    :: Kind
    , pairKicker1 :: Kind
    , pairKicker2 :: Kind
    , pairKicker3 :: Kind
    }
    deriving (Show, Eq, Ord)

data TwoPair = TwoPair -- ordered by rank of kind
    { twoPairHighPair :: Kind
    , twoPairLowPair  :: Kind
    , twoPairKicker   :: Kind
    }
    deriving (Show, Eq, Ord)

data ThreeOfAKind = ThreeOfAKind -- ordered by rank of kind
    { threeOfAKindKind    :: Kind
    , threeOfAKindKicker1 :: Kind
    , threeOfAKindKicker2 :: Kind
    }
    deriving (Show, Eq, Ord)

{- HLINT ignore Straight -}
data Straight = Straight
    { straightKind :: Kind -- hightest card
    }
    deriving (Show, Eq, Ord)

data Flush = Flush -- ordered by rank of kind
    { flush1 :: Kind
    , flush2 :: Kind
    , flush3 :: Kind
    , flush4 :: Kind
    , flush5 :: Kind
    }
    deriving (Show, Eq, Ord)

data FullHouse = FullHouse
    { fullHouseThreeOfAKind :: Kind
    , fullHousePair         :: Kind
    }
    deriving (Show, Eq, Ord)

data FourOfAKind = FourOfAKind
    { fourOfAKindKind   :: Kind
    , fourOfAKindKicker :: Kind
    }
    deriving (Show, Eq, Ord)

{- HLINT ignore StraightFlush -}
data StraightFlush = StraightFlush
    { straightFlushKind :: Kind -- highest card
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