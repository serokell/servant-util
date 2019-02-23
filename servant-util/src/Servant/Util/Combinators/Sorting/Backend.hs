-- | Applying sorting specifications.
module Servant.Util.Combinators.Sorting.Backend
    ( SortingBackend (..)
    , fieldSort
    , SortingApp (..)
    , SortingSpecApp

    , ApplyToSortItem (..)
    , backendApplySorting
    ) where

import Universum

import GHC.TypeLits (KnownSymbol)

import Servant.Util.Combinators.Sorting.Base
import Servant.Util.Common

-- | Implementation of filtering backend.
class SortingBackend backend where

    -- | The part of object which we are filtering on,
    -- provided by server backend implementor.
    type SortedValue backend a :: *

    -- | What we require from sorted values in order to be sortable.
    type SortedValueConstraint backend a :: Constraint
    type SortedValueConstraint backend a = ()

    -- | A resulting ordering.
    type BackendOrdering backend :: *

    -- | Implement 'SortingApp' as sorting on the given field.
    backendFieldSort
        :: SortedValueConstraint backend a
        => SortedValue backend a
        -> SortingApp backend ('TyNamedParam name a)

fieldSort
    :: forall name a backend.
       (SortingBackend backend, SortedValueConstraint backend a)
    => SortedValue backend a -> SortingApp backend ('TyNamedParam name a)
fieldSort = backendFieldSort

-- | A function defining a way to apply the given 'SortingItem' (which is sorting
-- order on a single parameter).
newtype SortingApp backend param
    = SortingApp (SortingItemTagged param -> BackendOrdering backend)

{- | List of 'SortingApp' functions. Describes how to apply @SortingSpec params@
(each of possible 'SortingItem') to an SQL query.

Instance of this type can be created using 'fieldSort' function. For example:

@
sortingSpecApp :: SortingSpecApp ["course" ?: Course, "desc" ?: Text]
sortingSpecApp =
    fieldSort @"course" courseField .*.
    fieldSort @"desc" descField .*.
    HNil
@

Annotating 'fieldSort' call with parameter name is not mandatory but recommended
to prevent possible mistakes in 'fieldSort's ordering.
-}
type SortingSpecApp backend (params :: [TyNamedParam *]) =
    HList (SortingApp backend) params

-- | Lookup for appropriate 'SortingApp' in 'SortingSpecApp' and apply it to 'SortingItem'.
class ApplyToSortItem backend params where
    -- | Apply spec app to the given 'SortingItem'
    -- We return 'Maybe' here (instead of forcing presence at type-level) for convenience.
    applyToSortItem
        :: SortingSpecApp backend params
        -> SortingItem
        -> Maybe (BackendOrdering backend)

instance ApplyToSortItem backend '[]  where
    applyToSortItem HNil _ = Nothing

instance (KnownSymbol name, ApplyToSortItem backend params) =>
         ApplyToSortItem backend ('TyNamedParam name p ': params) where
    applyToSortItem (SortingApp app `HCons` appRem) item = asum
        [ guard (symbolValT @name == siName item) $> app (SortingItemTagged item)
        , applyToSortItem @backend @params appRem item
        ]

-- | Apply a given 'SortingSpecApp' to a 'SortingSpec' producing a pack of
-- ordering values which define lexicographical sorting order.
backendApplySorting
    :: forall params backend.
       ApplyToSortItem backend params
    => SortingSpec params
    -> SortingSpecApp backend params
    -> [BackendOrdering backend]
backendApplySorting spec app =
    unSortingSpec spec <&> \sitem ->
        applyToSortItem @backend @params app sitem
           -- impossible due to invariants of 'SortingSpec'
        ?: error ("Impossible: don't know how to apply to spec item " <> show sitem)
