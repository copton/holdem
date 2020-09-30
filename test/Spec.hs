import Test.Tasty

import qualified TestCombinations as TestCombinations
import qualified TestHands as TestHands
import qualified PropHands as PropHands

main :: IO ()
main = defaultMain $ testGroup "holdem tests"
    [ TestCombinations.tests
    , TestHands.tests
    , PropHands.tests
    ]
