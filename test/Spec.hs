import Test.Tasty

import qualified TestCombinations as TestCombinations
import qualified QuickPropHands as QuickPropHands
import Test.DocTest (doctest)

main :: IO ()
main = do
    doctest ["-isrc", "src/Cards.hs", "src/Hands.hs", "src/Showdown.hs"]
    defaultMain $ testGroup "holdem tests"
        [ TestCombinations.tests
        , QuickPropHands.tests
        ]