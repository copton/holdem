module TestHands (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit 
import Data.Maybe (mapMaybe)

import Hands
import Cards

tests :: TestTree
tests = testGroup "hands tests" $
    [ testGroup "combinations" $
        map runCombinationTest $
               combinationTests
            ++ mapMaybe flipCombinationTest combinationTests
    ]

type CombinationTest = (String, Combination, Combination, Ordering)

runCombinationTest :: CombinationTest -> TestTree
runCombinationTest (label, left, right, result) = testCase label $
    result @=? compare left right 

flipCombinationTest :: CombinationTest -> Maybe CombinationTest
flipCombinationTest (_, _, _, EQ) = Nothing
flipCombinationTest (label, left, right, res) =
      Just (label ++ " flipped", right, left, flipRes res)
    where
        flipRes GT = LT
        flipRes LT = GT
        flipRes EQ = EQ

combinationTests :: [CombinationTest]
combinationTests =
    [ ( "Ace high"
      , CHighCard (HighCard $ orderedDesc [Ace, King, Queen, Nine, Five])
      , CHighCard (HighCard $ orderedDesc [King, Queen, Seven, Three, Two])
      , GT
      )
    , ( "Kicker"
      , CPair (Pair King $ orderedDesc [Ace, Four, Three])
      , CPair (Pair King $ orderedDesc [Ten, Seven, Five])
      , GT
      )
    , ( "Cooler"
      , CFullHouse (FullHouse King Queen)
      , CFourOfAKind (FourOfAKind King)
      , LT
      )
    ]