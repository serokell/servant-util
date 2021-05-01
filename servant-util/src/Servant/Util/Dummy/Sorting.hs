{- | Implements plain lexicographical sorting.

Example:

@
sortingSpecApp
    :: MyObject
    -> SortingSpecApp
        DummySortingBackend
        [ "id" ?: Int
        , "desc" ?: Text
        ]
sortingSpecApp obj =
    fieldSort @"id" (id obj) .*.
    fieldSort @"desc" (desc obj) .*.
    HNil
@

Next, you use `sortBySpec` to apply sorting.

@
sortObjects sorting = filter (sortBySpec sorting . sortingSpecApp) allObjects
@

-}
module Servant.Util.Dummy.Sorting
    ( SortingSpecApp
    , fieldSort
    , sortBySpec
    ) where

import Universum

import Data.Typeable (cast)

import Servant.Util.Combinators.Sorting.Backend
import Servant.Util.Combinators.Sorting.Base

-- | Implements sorting for beam-postgres package.
data DummySortingBackend

data SomeOrd = forall a. (Typeable a, Ord a) => SomeOrd a

-- | Unsafe instance which assumes that 'SomeOrd' contains the same items inside.
instance Eq SomeOrd where
    (==) = (== EQ) ... compare

-- | Unsafe instance which assumes that 'SomeOrd' contains the same items inside.
instance Ord SomeOrd where
    SomeOrd a `compare` SomeOrd b =
        let b' = cast b ?: error "Compared `SomeOrd`s are different inside"
        in a `compare` b'

instance SortingBackend DummySortingBackend where
    type SortedValue DummySortingBackend a = a
    type BackendOrdering DummySortingBackend = SomeOrd

    type SortedValueConstraint DummySortingBackend a = (Typeable a, Ord a)

    backendFieldSort field = SortingApp $ \(SortingItemTagged (SortingItem _name order)) ->
        case order of
            Ascendant  -> SomeOrd field
            Descendant -> SomeOrd (Down field)

-- | Applies a whole filtering specification to a set of response fields.
-- Resulting value can be put to 'filter' function.
sortBySpec
    :: ( backend ~ DummySortingBackend
       , allParams ~ AllSortingParams provided base
       , ApplyToSortItem backend allParams
       )
    => SortingSpec provided base
    -> (a -> SortingSpecApp backend allParams)
    -> [a] -> [a]
sortBySpec spec mkApp values =
    map fst . sortOn snd $
    map (id &&& backendApplySorting spec . mkApp) values
