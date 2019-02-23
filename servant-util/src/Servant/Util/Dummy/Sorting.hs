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

import Servant.Util.Combinators.Sorting.Backend
import Servant.Util.Combinators.Sorting.Base


data DummySortingBackend

data SomeOrd = forall a. Ord a => SomeOrd a

instance SortingBackend DummySortingBackend where
    type SortedValue DummySortingBackend a = a
    type BackendOrdering DummySortingBackend = SomeOrd

    type SortedValueConstraint DummySortingBackend a = Ord a

    fieldSort field = SortingApp $ \(SortingItemTagged SortingItem{..}) ->
        case siOrder of
                Ascendant  -> SomeOrd field
                Descendant -> SomeOrd (Down field)

ordering
    :: (ApplyToSortItem backend params, backend ~ DummySortingBackend)
    => SortingSpec params -> SortingSpecApp backend params -> [SomeOrd]
ordering = backendApplySorting

-- | Applies a whole filtering specification to a set of response fields.
-- Resulting value can be put to 'filter' function.
sortBySpec
    :: (ApplyToSortItem backend params, backend ~ DummySortingBackend)
    => SortingSpec params -> (a -> SortingSpecApp backend params) -> [a] -> [a]
sortBySpec spec mkApp values =
    map fst . sortOn snd $
    map (id &&& ordering spec . mkApp) values
