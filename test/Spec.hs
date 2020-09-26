import Test.Tasty

import qualified TestHands as TestHands

main :: IO ()
main = defaultMain $ testGroup "holdem tests"
    [ TestHands.tests
    ]
