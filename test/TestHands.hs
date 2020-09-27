module TestHands (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit 

import Hands
import Cards
import Combinations

tests :: TestTree
tests = testGroup "hands tests" [
    testGroup "five cards" $ map runHandTest handTestsFiveCards
    ]

type HandTest = (String, [Card] -> Maybe Combination, [Card], Maybe Combination)

runHandTest :: HandTest -> TestTree
runHandTest (label, f, cards, result) =
    testCase label $
        result @=? f cards

handTestsFiveCards :: [HandTest]
handTestsFiveCards =
    [ ( "Flush"
      , isFlush
      , [ Card King Clubs, Card Ten Clubs, Card Eight Clubs
        , Card Ace Clubs, Card Two Clubs
        ]
      , Just $ CFlush $ Flush $ orderDesc [King, Ten, Eight, Ace, Two]
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
    ]