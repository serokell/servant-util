-- | Converting a sorting specification to a value understandable by Beam.
module Servant.Util.Beam.Postgres.Sorting
    ( SortingSpecApp
    , fieldSort
    , sortBy_
    ) where

import Universum

import Data.Coerce (coerce)
import Database.Beam.Backend.SQL.SQL92 (IsSql92OrderingSyntax, Sql92OrderingExpressionSyntax,
                                        Sql92SelectExpressionSyntax, Sql92SelectOrderingSyntax)
import Database.Beam.Query (SqlOrderable, asc_, desc_, orderBy_)
import Database.Beam.Query.Internal (Projectible, Q, QExpr, QNested, QOrd, ThreadRewritable,
                                     WithRewrittenThread)

import Servant.Util.Combinators.Sorting.Backend
import Servant.Util.Combinators.Sorting.Base

-- | Implements sorting for beam-postgres package.
data BeamSortingBackend syntax s

instance IsSql92OrderingSyntax syntax =>
         SortingBackend (BeamSortingBackend syntax s) where

    type SortedValue (BeamSortingBackend syntax s) a =
        QExpr (Sql92OrderingExpressionSyntax syntax) s a

    type BackendOrdering (BeamSortingBackend syntax s) =
        QOrd syntax s Void

    backendFieldSort field = SortingApp $ \(SortingItemTagged (SortingItem _name order)) ->
        let applyOrder = case order of
                Ascendant  -> asc_
                Descendant -> desc_

        -- TODO [DSCP-425]
        -- Ordering NULLs is not supported by SQLite :peka:
        -- nullsOrder = case siNullsOrder of
        --     Nothing         -> id
        --     Just NullsFirst -> nullsFirst_
        --     Just NullsLast  -> nullsLast_

        in applyOrder (coerce field)

-- | Applies 'orderBy_' according to the given sorting specification.
sortBy_
    :: ( backend ~ BeamSortingBackend syntax0 s0
       , ApplyToSortItem backend params
       , Projectible (Sql92SelectExpressionSyntax syntax) a
       , SqlOrderable (Sql92SelectOrderingSyntax syntax) (BackendOrdering backend)
       , ThreadRewritable (QNested s) a
       )
    => SortingSpec params
    -> (a -> SortingSpecApp backend params)
    -> Q syntax db (QNested s) a
    -> Q syntax db s (WithRewrittenThread (QNested s) s a)
sortBy_ spec app = orderBy_ (backendApplySorting spec . app)
