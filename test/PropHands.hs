{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PropHands (
    tests
) where

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import GHC.Generics
import Data.List (nub, sort)
import Data.Maybe (fromJust)

import Cards
import Hands
import Combinations

deriving instance Generic Kind
instance (Monad m) => (Serial m) Kind

deriving instance Generic Suit
instance (Monad m) => (Serial m) Suit

tests :: TestTree
tests = testGroup "hands properties" [
    testProperty "High Card" propHighCard
    ]

propHighCard
    :: Monad m
    => Kind
    -> Kind
    -> Kind
    -> Kind
    -> Kind
    -> Property m
propHighCard k1 k2 k3 k4 k5 =
        noPairs
     && noStraight
    ==> case bestCombination hand of
            CHighCard _ -> True
            _           -> False
    where
        noPairs = length (nub [k1, k2, k3, k4, k5]) == 5
        noStraight =
            let range = sort $ map fromEnum [k1, k2, k3, k4, k5]
                min = range !! 0
                max = range !! 4
            in range /= [min .. max]
        hand = fromJust $ asHand 
                [ Card k1 Hearts
                , Card k2 Hearts
                , Card k3 Hearts
                , Card k4 Hearts
                , Card k5 Diamonds
                ]

--x y = x /= y ==> x /= y