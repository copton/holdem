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

instance Ord Card where
    compare (Card k1 s1) (Card k2 s2) =
        (fromEnum k1, fromEnum s1) `compare` (fromEnum k2, fromEnum s2)


-- | Enumerate all cards, first by kind then by suit
--
-- Examples:
-- >>> length ([minBound .. maxBound] :: [Card])
-- 52
--
-- >>> minBound :: Card
-- Card {cardKind = Two, cardSuit = Hearts}
--
-- >>> toEnum 12 :: Card
-- Card {cardKind = Ace, cardSuit = Hearts}
--
-- >>> maxBound :: Card
-- Card {cardKind = Ace, cardSuit = Clubs}

instance Bounded Card where
    minBound = Card minBound minBound
    maxBound = Card maxBound maxBound

numberofKinds :: Int
numberofKinds = fromEnum (maxBound :: Kind) + 1

instance Enum Card where
    toEnum n =
        let (d, m) = divMod n numberofKinds
        in  Card (toEnum m) (toEnum d)

    fromEnum (Card k s) =
        (fromEnum s) * numberofKinds + (fromEnum k)
