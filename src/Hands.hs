module Hands where

import Cards

data Hand = Hand [Card]
    deriving (Show, Eq)

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


data HighCard = HighCard
    { hcOrdered :: [Kind] -- | highest to lowest
    }
    deriving (Show, Eq, Ord)

data Pair = Pair
    { pairKind     :: Kind
    , pairKicker   :: [Kind] -- | highest to lowest
    }
    deriving (Show, Eq, Ord)

data TwoPair = TwoPair
    { twoPairHighPair :: Kind
    , twoPairLowPair  :: Kind
    , twoPairKicker   :: [Kind] -- | highest to lowest
    }
    deriving (Show, Eq, Ord)

data ThreeOfAKind = ThreeOfAKind
    { threeOfAKindKind   :: Kind
    , threeOfAKindKicker :: [Kind] -- | highest to lowest
    }
    deriving (Show, Eq, Ord)

data Straight = Straight
    { straightKind :: Kind -- | hightest card
    }
    deriving (Show, Eq, Ord)

data Flush = Flush
    { flushSuit :: Suit
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

instance Ord Hand where
    compare lhand rhand = EQ


