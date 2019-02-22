{- | Implements plain filtering.

Example:

@
filteringSpecApp
    :: MyObject
    -> FilteringSpecApp
        DummyFilterBackend
        [ "id" ?: 'AutoFilter Course
        , "desc" ?: 'AutoFilter Text
        , "isAwesome" ?: 'ManualFilter Bool
        ]
filteringSpecApp obj =
    filterOn_ @"id" (id obj) .*.
    filterOn_ @"desc" (desc obj) .*.
    customFilter_ @"isAwesome" (== (awesomeness obj > 42)) .*.
    HNil
@

Annotating 'filterOn' and 'customFilter' calls with parameter name is fully optional
and used only to visually disambiguate filters of the same types.

Next, you use `matches` to check whether a value matches user-provided filters.

@
filterObjects filters = filter (matches filters . filteringSpecApp) allObjects
@

-}
module Servant.Util.Dummy.Filtering
    ( matches
    ) where

import Universum

import Servant.Util.Combinators.Filtering

-- | Implements filters via Beam query expressions ('QExpr').
data DummyFilterBackend

instance FilterBackend DummyFilterBackend where
    type AutoFilteredValue DummyFilterBackend a = a
    type MatchPredicate DummyFilterBackend = Bool

instance Eq a => AutoFilterSupport DummyFilterBackend FilterMatching a where
    autoFilterSupport = \case
        FilterMatching v -> (== v)
        FilterNotMatching v -> (/= v)
        FilterItemsIn vs -> (`elem` vs)

instance Ord a => AutoFilterSupport DummyFilterBackend FilterComparing a where
    autoFilterSupport = \case
        FilterGT v -> (> v)
        FilterLT v -> (< v)
        FilterGTE v -> (>= v)
        FilterLTE v -> (<= v)

-- | Applies a whole filtering specification to a set of response fields.
-- Resulting value can be put to 'filter' function.
matches
    :: ( backend ~ DummyFilterBackend
       , BackendApplySomeFilter backend params
       )
    => FilteringSpec params
    -> FilteringSpecApp backend params
    -> Bool
matches = and ... backendApplyFilters
