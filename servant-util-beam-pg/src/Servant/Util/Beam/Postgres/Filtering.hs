{- | Implements filtering with beam-postgres.

When setting filtering for an endpoint, you usually need to construct a filtering spec
application first, which describes how to perform filtering over your rows:

@
filteringSpecApp
    :: FilteringSpecApp
        (BeamFilterBackend syntax s)
        [ "course" ?: 'AutoFilter Course
        , "desc" ?: 'AutoFilter Text
        , "isAwesome" ?: 'ManualFilter Bool
        ]
filteringSpecApp =
    filterOn_ @"course" courseField .*.
    filterOn_ @"desc" descField .*.
    customFilter_ @"isAwesome"
        (\isAwesome -> (courseAwesomeness >. val_ 42) ==. val_ isAwesome) .*.
    HNil
@

Annotating 'filterOn' and 'customFilter' calls with parameter name is fully optional
and used only to visually disambiguate filters of the same types.

Next, you use `matches_` to build a filtering expression understandable by Beam.
-}
module Servant.Util.Beam.Postgres.Filtering
    ( matches_

      -- * Re-exports
    , filterOn
    , manualFilter
    ) where

import Universum

import Database.Beam.Backend.SQL (HasSqlValueSyntax, IsSql92ExpressionSyntax,
                                  Sql92ExpressionValueSyntax)
import Database.Beam.Query (HasSqlEqualityCheck, in_, val_, (&&.), (/=.), (<.), (<=.), (==.), (>.),
                            (>=.))
import Database.Beam.Query.Internal (QExpr)

import Servant.Util.Combinators.Filtering

-- | Implements filters via Beam query expressions ('QExpr').
data BeamFilterBackend (syntax :: *) (s :: *)

instance IsSql92ExpressionSyntax syntax =>
         FilterBackend (BeamFilterBackend syntax s) where

    type AutoFilteredValue (BeamFilterBackend syntax s) a =
        QExpr syntax s a

    type MatchPredicate (BeamFilterBackend syntax s) =
        QExpr syntax s Bool

    trueMatchPreducate = val_ True

    conjunctMatchPredicates = (&&.)

instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a
         , HasSqlEqualityCheck syntax a
         ) =>
         AutoFilterSupport (BeamFilterBackend syntax s) FilterMatching a where
    autoFilterSupport = \case
        FilterMatching v -> (==. val_ v)
        FilterNotMatching v -> (/=. val_ v)
        FilterItemsIn vs -> (`in_` map val_ vs)

instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a
         , IsSql92ExpressionSyntax syntax
         ) =>
         AutoFilterSupport (BeamFilterBackend syntax s) FilterComparing a where
    autoFilterSupport = \case
        FilterGT v -> (>. val_ v)
        FilterLT v -> (<. val_ v)
        FilterGTE v -> (>=. val_ v)
        FilterLTE v -> (<=. val_ v)

-- | Applies a whole filtering specification to a set of response fields.
-- Resulting value can be put to 'guard_' or 'filter_' function.
matches_
    :: ( backend ~ BeamFilterBackend syntax s
       , BackendApplySomeFilter backend params
       )
    => FilteringSpec params
    -> FilteringSpecApp backend params
    -> QExpr syntax s Bool
matches_ = backendApplyFilters
