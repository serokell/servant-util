-- | Converting a sorting specification to a value understandable by Beam.
module Servant.Util.Beam.Postgres.Sorting
    ( SortingSpecApp
    , fieldSort
    , sortBy_
    ) where

import Universum

import Data.Coerce (coerce)
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Query (SqlOrderable, asc_, desc_, orderBy_)
import Database.Beam.Query.Internal (Projectible, Q, QExpr, QNested, QOrd, ThreadRewritable,
                                     WithRewrittenThread)

import Servant.Util.Combinators.Sorting.Backend
import Servant.Util.Combinators.Sorting.Base

-- | Implements sorting for beam-postgres package.
data BeamSortingBackend be s

instance (BeamSqlBackend be) =>
         SortingBackend (BeamSortingBackend be s) where

    type SortedValue (BeamSortingBackend be s) a =
        QExpr be s a

    type BackendOrdering (BeamSortingBackend be s) =
        QOrd be s Void

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
       , allParams ~ AllSortingParams provided base
       , ApplyToSortItem backend allParams
       , Projectible be a
       , SqlOrderable be (BackendOrdering backend)
       , ThreadRewritable (QNested s) a
       )
    => SortingSpec provided base
    -> (a -> SortingSpecApp backend allParams)
    -> Q be db (QNested s) a
    -> Q be db s (WithRewrittenThread (QNested s) s a)
sortBy_ spec app = orderBy_ (backendApplySorting spec . app)
