-- | Converting a sorting specification to a value understandable by Beam.
module Servant.Util.Beam.Postgres.Sorting
    ( SortingSpecApp (..)
    , fieldSort_
    , bySpec_
    ) where

import Universum

import Data.Coerce (coerce)
import Database.Beam.Backend.SQL.SQL92 (IsSql92OrderingSyntax, Sql92OrderingExpressionSyntax)
import Database.Beam.Query (asc_, desc_)
import Database.Beam.Query.Internal (QExpr, QOrd)
import GHC.TypeLits (KnownSymbol)

import Servant.Util.Combinators.Sorting
import Servant.Util.Common

-- | Under the hood we don't really care about the type we are sorting on.
type SomeQOrd syntax s = QOrd syntax s Void

-- | A function defining a way to apply the given 'SortingItem' (which is sorting
-- order on a single parameter) as a part of Beam's 'orderBy_'.
type SortingToBeam syntax s (param :: TyNamedParam *) =
    SortingItemTagged param -> SomeQOrd syntax s

{- | List of 'SortingToBeam' functions. Describes how to apply @SortingSpec params@
(each of possible 'SortingItem') to an SQL query.

Instance of this type can be created using 'fieldSort_' function. For example:

@
let defSortingSpecApp :: SortingSpecApp ["course" ?: Course, "desc" ?: Text]
    defSortingSpecApp =
        fieldSort_ @"course" courseField .*.
        fieldSort_ @"desc" descField .*.
        hEnd
@

Annotating 'fieldSort_' call with parameter name is not mandatory but recommended
to prevent possible mistakes in 'fieldSort_'s ordering.
-}
data SortingSpecApp syntax s (params :: [TyNamedParam *]) where
    SortingSpecAppEnd :: SortingSpecApp syntax s '[]
    (:>:)
        :: SortingToBeam syntax s param
        -> SortingSpecApp syntax s params
        -> SortingSpecApp syntax s (param ': params)
infixr 3 :>:

-- | Implement 'SortingToBeam' as sorting on the given table field.
fieldSort_
    :: forall name a syntax s.
       (IsSql92OrderingSyntax syntax)
    => QExpr (Sql92OrderingExpressionSyntax syntax) s a
    -> SortingToBeam syntax s ('TyNamedParam name a)
fieldSort_ field (SortingItemTagged SortingItem{..}) = order (coerce field)
  where
    order = case siOrder of
        Ascendant  -> asc_
        Descendant -> desc_

    -- TODO [DSCP-425]
    -- Ordering NULLs is not supported by SQLite :peka:
    -- nullsOrder = case siNullsOrder of
    --     Nothing         -> id
    --     Just NullsFirst -> nullsFirst_
    --     Just NullsLast  -> nullsLast_

-- | Lookup for appropriate 'SortingToBeam' in 'SortingSpecApp' and apply it to 'SortingItem'.
class ApplyToSortItem syntax s params where
    -- | Apply spec app to the given 'SortingItem'
    -- We return 'Maybe' here (instead of forcing presence at type-level) for convenience.
    applyToSortItem
        :: SortingSpecApp syntax s params
        -> SortingItem
        -> Maybe (SomeQOrd syntax s)

instance ApplyToSortItem syntax s '[]  where
    applyToSortItem SortingSpecAppEnd _ = Nothing

instance (KnownSymbol name, ApplyToSortItem syntax s params) =>
         ApplyToSortItem syntax s ('TyNamedParam name p ': params) where
    applyToSortItem (app :>: appRem) item = asum
        [ guard (symbolValT @name == siName item) $> app (SortingItemTagged item)
        , applyToSortItem @syntax @s @params appRem item
        ]

-- | Apply a given 'SortingSpecApp' to a 'SortingSpec' producing a value which can be put
-- to 'orderBy_'.
bySpec_
    :: forall params syntax s.
       ApplyToSortItem syntax s params
    => SortingSpec params
    -> SortingSpecApp syntax s params
    -> [SomeQOrd syntax s]
bySpec_ spec app =
    unSortingSpec spec <&> \sitem ->
        applyToSortItem @syntax @s @params app sitem
           -- impossible due to invariants of 'SortingSpec'
        ?: error ("Impossible: don't know how to apply to spec item " <> show sitem)
