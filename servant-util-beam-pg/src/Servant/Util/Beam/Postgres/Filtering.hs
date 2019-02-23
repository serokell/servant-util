{- | Implements filtering with beam-postgres.

When setting filtering for an endpoint, you usually need to construct a filtering spec
application first, which describes how to perform filtering over your rows:

@
filteringSpecApp
    :: FilteringSpecApp
        (QExprFilterBackend syntax s)
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

Next, you use 'matches_' or 'filterGuard_' to build a filtering expression understandable
by Beam.
-}
module Servant.Util.Beam.Postgres.Filtering
    ( matches_
    , filtersGuard_
    , filterOn
    , manualFilter

      -- * Internals
    , likeToSqlPattern
    ) where

import Universum

import Database.Beam.Backend.SQL (HasSqlValueSyntax, IsSql92ExpressionSyntax, IsSql92SelectSyntax,
                                  IsSqlExpressionSyntaxStringType, Sql92ExpressionValueSyntax,
                                  Sql92SelectSelectTableSyntax, Sql92SelectTableExpressionSyntax)
import Database.Beam.Query (HasSqlEqualityCheck, Q, guard_, in_, like_, val_, (&&.), (/=.), (<.),
                            (<=.), (==.), (>.), (>=.))
import Database.Beam.Query.Internal (QExpr)

import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Combinators.Filtering.Backend
import Servant.Util.Combinators.Filtering.Filters

-- | Implements filters via Beam query expressions ('QExpr').
data QExprFilterBackend syntax s

instance FilterBackend (QExprFilterBackend syntax s) where

    type AutoFilteredValue (QExprFilterBackend syntax s) a =
        QExpr syntax s a

    type MatchPredicate (QExprFilterBackend syntax s) =
        QExpr syntax s Bool

instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a
         , HasSqlEqualityCheck syntax a
         ) =>
         AutoFilterSupport (QExprFilterBackend syntax s) FilterMatching a where
    autoFilterSupport = \case
        FilterMatching v -> (==. val_ v)
        FilterNotMatching v -> (/=. val_ v)
        FilterItemsIn vs -> (`in_` map val_ vs)

instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a
         , IsSql92ExpressionSyntax syntax
         ) =>
         AutoFilterSupport (QExprFilterBackend syntax s) FilterComparing a where
    autoFilterSupport = \case
        FilterGT v -> (>. val_ v)
        FilterLT v -> (<. val_ v)
        FilterGTE v -> (>=. val_ v)
        FilterLTE v -> (<=. val_ v)

-- For now we do not support custom escape characters.
pattern PgEsc :: Char
pattern PgEsc = '\\'

likeToSqlPattern :: LikePattern -> String
likeToSqlPattern = go . toString . unLikePattern
  where
    go = \case
        Esc : '.' : r -> '.' : go r
        Esc : '*' : r -> '*' : go r
        Esc : c : r -> Esc : c : go r

        '_' : r -> PgEsc : '_' : go r
        '%' : r -> PgEsc : '%' : go r

        '.' : r -> '_' : go r
        '*' : r -> '%' : go r

        c : r -> c : go r
        [] -> []

instance ( IsSql92ExpressionSyntax syntax
         , IsString text
         , IsSqlExpressionSyntaxStringType syntax text
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) text
         ) =>
         AutoFilterSupport (QExprFilterBackend syntax s) FilterLike text where
    autoFilterSupport = \case
        FilterLike _cs pat ->
            let sqlPat = fromString $ likeToSqlPattern pat
            in (`like_` val_ sqlPat)
      where

-- | Applies a whole filtering specification to a set of response fields.
-- Resulting value can be put to 'guard_' or 'filter_' function.
matches_
    :: ( backend ~ QExprFilterBackend syntax s
       , BackendApplySomeFilter backend params
       , IsSql92ExpressionSyntax syntax
       )
    => FilteringSpec params
    -> FilteringSpecApp backend params
    -> QExpr syntax s Bool
matches_ = foldr (&&.) (val_ True) ... backendApplyFilters

-- | Implements filters via Beam query monad ('Q').
data QFilterBackend syntax (db :: (* -> *) -> *) s

instance FilterBackend (QFilterBackend syntax db s) where

    type AutoFilteredValue (QFilterBackend syntax db s) a =
        QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax syntax)) s a

    type MatchPredicate (QFilterBackend syntax db s) =
        Q syntax db s ()

instance ( select ~ Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax syntax)
         , IsSql92SelectSyntax syntax
         , AutoFilterSupport (QExprFilterBackend select s) filter a
         ) =>
         AutoFilterSupport (QFilterBackend syntax db s) filter a where
    autoFilterSupport =
        guard_ ... autoFilterSupport @(QExprFilterBackend select s)

-- | Applies a whole filtering specification to a set of response fields.
-- Resulting value can be monadically binded with the remaining query (just like 'guard_').
filtersGuard_
    :: ( backend ~ QFilterBackend syntax db s
       , BackendApplySomeFilter backend params
       )
    => FilteringSpec params
    -> FilteringSpecApp backend params
    -> Q syntax db s ()
filtersGuard_ = sequence_ ... backendApplyFilters
