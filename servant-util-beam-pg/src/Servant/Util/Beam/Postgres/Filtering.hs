{-# LANGUAGE TypeInType #-}

{- | Implements filtering with beam-postgres.

When setting filtering for an endpoint, you usually need to construct a filtering spec
application first, which describes how to performing filtering over your rows:

@
filteringSpecApp
    :: FilteringSpecApp
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

Annotating 'filterOn_' and 'customFilter_' calls with parameter name is fully optional
and used only to visually disambiguate filters of the same types.

Next, you use `applyFilters_` to build a filtering expression understandable by Beam.
-}
module Servant.Util.Beam.Postgres.Filtering
    ( filterOn_
    , manualFilter_
    , applyFilters_
    ) where

import Universum

import Data.Kind (type (*))
import Database.Beam.Backend.SQL (HasSqlValueSyntax, IsSql92ExpressionSyntax,
                                  Sql92ExpressionValueSyntax)
import Database.Beam.Query (HasSqlEqualityCheck, in_, val_, (&&.), (/=.), (<.), (<=.), (==.), (>.),
                            (>=.))
import Database.Beam.Query.Internal (QExpr)

import Servant.Util.Combinators.Filtering
import Servant.Util.Common

-- | Implements filters via Beam query expressions ('QExpr').
data BeamFilterBackend (syntax :: *) (s :: *)

instance IsSql92ExpressionSyntax syntax =>
         FilterBackend (BeamFilterBackend syntax s) where

    type AutoFilteredValue (BeamFilterBackend syntax s) a =
        QExpr syntax s a

    newtype MatchPredicate (BeamFilterBackend syntax s) =
        BeamMatchPredicate
        { unBeamMatchPredicate :: QExpr syntax s Bool
        }

instance IsSql92ExpressionSyntax syntax =>
         Semigroup (MatchPredicate (BeamFilterBackend syntax s)) where
    BeamMatchPredicate a <> BeamMatchPredicate b = BeamMatchPredicate (a &&. b)

instance IsSql92ExpressionSyntax syntax =>
         Monoid (MatchPredicate (BeamFilterBackend syntax s)) where
    mempty = BeamMatchPredicate $ val_ True
    mappend = (<>)

instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a
         , HasSqlEqualityCheck syntax a
         ) =>
         AutoFilterSupport (BeamFilterBackend syntax s) FilterMatching a where
    autoFilterSupport = BeamMatchPredicate ... \case
        FilterMatching v -> (==. val_ v)
        FilterNotMatching v -> (/=. val_ v)
        FilterItemsIn vs -> (`in_` map val_ vs)

instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a
         , IsSql92ExpressionSyntax syntax
         ) =>
         AutoFilterSupport (BeamFilterBackend syntax s) FilterComparing a where
    autoFilterSupport = BeamMatchPredicate ... \case
        FilterGT v -> (>. val_ v)
        FilterLT v -> (<. val_ v)
        FilterGTE v -> (>=. val_ v)
        FilterLTE v -> (<=. val_ v)

-- | Applies a whole filtering specification to a set of response fields.
-- Resulting value can be put to 'guard_' or 'filter_' function.
applyFilters_
    :: (BackendApplySomeFilter backend params, backend ~ BeamFilterBackend syntax s)
    => FilteringSpec params
    -> FilteringSpecApp backend params
    -> QExpr syntax s Bool
applyFilters_ = unBeamMatchPredicate ... backendApplyFilters

-- | Implement an automatic filter.
-- User-provided filtering operation will do filter on this value.
filterOn_
    :: forall name a syntax s.
       (Typeable a)
    => QExpr syntax s a
    -> FilteringApp (BeamFilterBackend syntax s) ('TyNamedParam name ('AutoFilter a))
filterOn_ = AutoFilteringApp

-- | Implement a manual filter.
-- You are provided with a value which user supplied and so you have
-- to construct a Beam predicate involving that value and relevant response fields.
manualFilter_
    :: forall name a syntax s.
       (Typeable a)
    => (a -> QExpr syntax s Bool)
    -> FilteringApp (BeamFilterBackend syntax s) ('TyNamedParam name ('ManualFilter a))
manualFilter_ filtr = ManualFilteringApp (BeamMatchPredicate . filtr)
