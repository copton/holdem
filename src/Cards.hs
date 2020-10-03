module Cards
    ( Kind(..)
    , Suit(..)
    , Card(..)
    )
where

data Kind
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    deriving (Show, Eq, Ord, Enum, Bounded)

data Suit
    = Hearts
    | Spades
    | Diamonds
    | Clubs
    deriving (Show, Eq, Enum, Bounded)

instance Ord Suit where
    compare _ _ = EQ

data Card = Card
    { cardKind :: Kind
    , cardSuit :: Suit
    } deriving (Show, Eq)
