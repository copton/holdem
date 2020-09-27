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
    [ testGroup "histogram" $ map runHistTest histTests
    , testGroup "five cards" $ map runHandTest handTestsFiveCards
    ]
  
type HistTest = (String, [Card], [(Kind, Int)])

runHistTest :: HistTest -> TestTree
runHistTest (label, cards, result) =
  testCase label $
    result @=? histogram cards

histTests :: [HistTest]
histTests =
    [ ("empty", [], [])
    , ("single", [Card Jack Hearts], [(Jack, 1)])
    , ( "pair"
      , [Card Jack Hearts, Card Jack Spades]
      , [(Jack, 2)]
      )
    , ( "pair with kicker"
      , [ Card King Hearts, Card King Clubs, Card Jack Clubs
        , Card Ten Diamonds, Card Queen Hearts]
      , [ (King, 2)
        , (Queen, 1)
        , (Jack, 1)
        , (Ten, 1)
        ]
      )
    ]

type HandTest = (String, Hand -> Maybe Combination, [Card], Maybe Combination)

runHandTest :: HandTest -> TestTree
runHandTest (label, f, cards, result) =
    testCase label $ do
      let Just hand = asHand cards
      result @=? f hand

handTestsFiveCards :: [HandTest]
handTestsFiveCards =
    [ ( "Flush"
      , isFlush
      , [ Card King Clubs, Card Ten Clubs, Card Eight Clubs                      
        , Card Ace Clubs, Card Two Clubs
        ]
      , Just $ CFlush $ Flush Ace King Ten Eight Two
      )
    , ( "not a Flush"
      , isFlush
      , [ Card King Hearts, Card Ten Clubs, Card Eight Clubs
        , Card Ace Clubs, Card Two Clubs]
      , Nothing
      )
    , ( "Straight"
      , isStraight
      , [ Card King Hearts, Card Nine Clubs, Card Jack Clubs
        , Card Ten Diamonds, Card Queen Hearts]
      , Just $ CStraight $ Straight King
      )
    , ( "not a Straight"
      , isStraight
      , [ Card Ace Hearts, Card Nine Clubs, Card Jack Clubs
        , Card Ten Diamonds, Card Queen Hearts]
      , Nothing
      )
    , ( "Straight Flush"
      , isStraightFlush
      , [ Card King Clubs, Card Nine Clubs, Card Jack Clubs
        , Card Ten Clubs, Card Queen Clubs]
      , Just $ CStraightFlush $ StraightFlush King
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
    , ( "FullHouse"
      , isFullHouse
      , [ Card King Hearts, Card King Clubs, Card Jack Clubs
        , Card King Diamonds, Card Jack Hearts]
      , Just $ CFullHouse $ FullHouse King Jack
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