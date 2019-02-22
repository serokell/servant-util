{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Manual construction of sorting spec.
module Servant.Util.Combinators.Sorting.Arbitrary () where

import Universum

import Test.QuickCheck (Arbitrary (..), elements, infiniteList, shuffle, sublistOf)

import Servant.Util.Combinators.Sorting.Base
import Servant.Util.Common


instance Arbitrary SortingOrder where
    arbitrary = elements [Ascendant, Descendant]

instance ReifyParamsNames params => Arbitrary (SortingSpec params) where
    arbitrary = do
        let names = toList $ reifyParamsNames @params
        someNames <- sublistOf =<< shuffle names
        orders <- infiniteList
        let sortItems = zipWith SortingItem someNames orders
        return (SortingSpec sortItems)
    shrink = map SortingSpec . reverse . inits . unSortingSpec
