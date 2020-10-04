import Test.Tasty

import qualified TestCombinations as TestCombinations
import qualified TestHands as TestHands
import qualified QuickPropHands as QuickPropHands
import Test.DocTest (doctest)

main :: IO ()
main = do
    doctest ["-isrc", "src/Cards.hs"]
    defaultMain $ testGroup "holdem tests"
        [ TestCombinations.tests
        , TestHands.tests
        , QuickPropHands.tests
        ]