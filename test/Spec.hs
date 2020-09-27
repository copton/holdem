import Test.Tasty

import qualified TestCombinations as TestCombinations
import qualified TestHands as TestHands

main :: IO ()
main = defaultMain $ testGroup "holdem tests"
    [ TestCombinations.tests
    , TestHands.tests
    ]
