module TestCombinations (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit 
import Data.Maybe (mapMaybe)

import Combinations
import Cards

tests :: TestTree
tests = testGroup "combinations tests" $
          map runCombinationTest $
                combinationTests
              ++ mapMaybe flipCombinationTest combinationTests

type CombinationTest = (String, Combination, Combination, Ordering)

runCombinationTest :: CombinationTest -> TestTree
runCombinationTest (label, left, right, result) =
    testCase label $
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
      , CHighCard (HighCard $ orderDesc [Ace, King, Queen, Nine, Five])
      , CHighCard (HighCard $ orderDesc [King, Queen, Seven, Three, Two])
      , GT
      )
    , ( "Kicker"
      , CPair (Pair King $ orderDesc [Ace, Four, Three])
      , CPair (Pair King $ orderDesc [Ten, Seven, Five])
      , GT
      )
    , ( "Cooler"
      , CFullHouse (FullHouse King Queen)
      , CFourOfAKind (FourOfAKind King)
      , LT
      )
    , ( "Chop"
      , CFullHouse (FullHouse King Queen)
      , CFullHouse (FullHouse King Queen)
      , EQ
      )
    , ( "Flush vs Flush"
      , CFlush (Flush $ orderDesc [Ace, King, Queen, Nine, Five])
      , CFlush (Flush $ orderDesc [King, Queen, Seven, Three, Two])
      , GT
      )
    ]