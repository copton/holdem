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
    deriving (Show, Eq, Ord)

data Suit
    = Heards
    | Spades
    | Diamonds
    | Clubs
    deriving (Show, Eq, Ord)

data Card = Card
    { cardKind :: Kind
    , cardSuit :: Suit
    } deriving (Show, Eq)
