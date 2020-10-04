module TestHands (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import Hands
import Cards
import Combinations

tests :: TestTree
tests = testGroup "hands tests"
    [ testGroup "five cards" $ map runHandTest handTestsFiveCards
    ]

type HandTest = (String, Hand -> Maybe Combination, [Card], Maybe Combination)

runHandTest :: HandTest -> TestTree
runHandTest (label, f, cards, result) =
    testCase label $ do
      let Just hand = asHand cards
      result @=? f hand

handTestsFiveCards :: [HandTest]
handTestsFiveCards =
    [ ( "not a Straight"
      , isStraight
      , [ Card Ace Hearts, Card Nine Clubs, Card Jack Clubs
        , Card Ten Diamonds, Card Queen Hearts]
      , Nothing
      )
    , ( "Straight 2"
      , isStraight
      , [ Card Three Hearts, Card Two Hearts, Card Four Hearts
        , Card Five Hearts, Card Six Hearts
        ]
      , Just $ CStraight $ Straight Six
      )
    , ( "Straight Flush 2"
      , isStraightFlush
      , [ Card Three Hearts, Card Two Hearts, Card Four Hearts
        , Card Five Hearts, Card Six Hearts
        ]
      , Just $ CStraightFlush $ StraightFlush Six
      )
    , ( "not a Straight Flush"
      , isStraightFlush
      , [ Card King Hearts, Card Nine Clubs, Card Jack Clubs
        , Card Ten Diamonds, Card Queen Hearts]
      , Nothing
      )
    , ( "Pair"
      , isPair
      , [ Card King Hearts, Card King Clubs, Card Jack Clubs
        , Card Ten Diamonds, Card Queen Hearts]
      , Just $ CPair $ Pair King Queen Jack Ten
      )
    , ( "Two Pair"
      , isTwoPair
      , [ Card King Hearts, Card King Clubs, Card Jack Clubs
        , Card Jack Diamonds, Card Queen Hearts]
      , Just $ CTwoPair $ TwoPair King Jack Queen
      )
    , ( "Three of a Kind"
      , isThreeOfAKind
      , [ Card King Hearts, Card King Clubs, Card Jack Clubs
        , Card King Diamonds, Card Queen Hearts]
      , Just $ CThreeOfAKind $ ThreeOfAKind King Queen Jack
      )
    , ( "Four of a Kind"
      , isFourOfAKind
      , [ Card King Hearts, Card King Clubs, Card Jack Clubs
        , Card King Diamonds, Card King Spades]
      , Just $ CFourOfAKind $ FourOfAKind King Jack
      )
    , ( "Two Pair is not Pair"
      , isPair
      , [ Card King Hearts, Card King Clubs, Card Jack Clubs
        , Card Jack Diamonds, Card Queen Hearts]
      , Nothing
      )
    , ( "Three of a Kind is not Pair"
      , isPair
      , [ Card King Hearts, Card King Clubs, Card Jack Clubs
        , Card King Diamonds, Card Queen Hearts]
      , Nothing
      )
    , ( "FullHouse is not Pair"
      , isPair
      , [ Card King Hearts, Card King Clubs, Card Jack Clubs
        , Card King Diamonds, Card Jack Hearts]
      , Nothing
      )
    , ( "For of a Kind is not Pair"
      , isPair
      , [ Card King Hearts, Card King Clubs, Card Jack Clubs
        , Card King Diamonds, Card King Spades]
      , Nothing
      )
    , ( "FullHouse is not Three of a Kind"
      , isThreeOfAKind
      , [ Card King Hearts, Card King Clubs, Card Jack Clubs
        , Card King Diamonds, Card Jack Hearts]
      , Nothing
      )
    , ( "For of a Kind is not Three of a Kind"
      , isThreeOfAKind
      , [ Card King Hearts, Card King Clubs, Card Jack Clubs
        , Card King Diamonds, Card King Spades]
      , Nothing
      )
    ]