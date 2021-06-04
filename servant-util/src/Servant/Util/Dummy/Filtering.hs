{- | Implements plain filtering.

Example:

@
filteringSpecApp
    :: MyObject
    -> FilteringSpecApp
        DummyFilteringBackend
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
    , filterBySpec
    , filterOn
    , manualFilter
    ) where

import Universum

import Data.Bits ((.|.))
import qualified Text.Regex.Posix as R
import qualified Text.Regex.Posix.String as RS

import Servant.Util.Combinators.Filtering.Backend
import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Combinators.Filtering.Filters

-- | Implements filters via Beam query expressions ('QExpr').
data DummyFilteringBackend

instance FilterBackend DummyFilteringBackend where
    type AutoFilteredValue DummyFilteringBackend a = a
    type MatchPredicate DummyFilteringBackend = Bool

instance Eq a => AutoFilterSupport DummyFilteringBackend FilterMatching a where
    autoFilterSupport = \case
        FilterMatching v    -> (== v)
        FilterNotMatching v -> (/= v)
        FilterItemsIn vs    -> (`elem` vs)

instance Ord a => AutoFilterSupport DummyFilteringBackend FilterComparing a where
    autoFilterSupport = \case
        FilterGT v  -> (> v)
        FilterLT v  -> (< v)
        FilterGTE v -> (>= v)
        FilterLTE v -> (<= v)

-- | Supported only for trivial cases yet.
instance ToString s => AutoFilterSupport DummyFilteringBackend FilterLike s where
    autoFilterSupport
      (FilterLike (CaseSensitivity cs) (LikePatternUnsafe (toString -> pat))) =
      \(toString -> txt) -> isJust @() $ do
        let compOpts = RS.compBlank
                & if cs then id else (.|. RS.compIgnoreCase)
        -- TODO: report this to servant parser
        regex <- R.makeRegexOptsM compOpts RS.execBlank (transformPat pat)
        R.matchM regex txt
      where
        transformPat s = '^' : replacePatChars s ++ "$"
        replacePatChars = \case
          []      -> []
          '*' : s -> '.' : '*' : replacePatChars s
          c : s   -> c : replacePatChars s

-- | Applies a whole filtering specification to a set of response fields.
-- Resulting value can be put to 'filter' function.
matches
    :: ( backend ~ DummyFilteringBackend
       , BackendApplySomeFilter backend params
       )
    => FilteringSpec params
    -> FilteringSpecApp backend params
    -> Bool
matches = and ... backendApplyFilters

-- | Filters given values by a filtering specification.
filterBySpec
    :: ( backend ~ DummyFilteringBackend
       , BackendApplySomeFilter backend params
       )
    => FilteringSpec params
    -> (a -> FilteringSpecApp backend params)
    -> [a]
    -> [a]
filterBySpec spec mkApp = filter (matches spec . mkApp)
