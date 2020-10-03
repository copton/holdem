import Test.Tasty

import qualified TestCombinations as TestCombinations
import qualified TestHands as TestHands
import qualified QuickPropHands as QuickPropHands

main :: IO ()
main = defaultMain $ testGroup "holdem tests"
    [ TestCombinations.tests
    , TestHands.tests
    , QuickPropHands.tests
    ]
