{-# LANGUAGE TypeInType #-}

-- | Provides base for filtering backend implementations.
module Servant.Util.Combinators.Filtering.Backend
    ( FilterBackend (..)
    , AutoFilterImpl
    , FilteringApp (..)
    , AutoFilterSupport (..)
    , FilteringSpecApp
    , BackendApplySomeFilter
    , typeAutoFiltersSupport
    , backendApplyFilters
    ) where

import Universum

import Data.Kind (type (*))
import Data.Typeable (cast, gcast1)
import GHC.TypeLits (KnownSymbol)

import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Common

-- | Implementation of filtering backend.
class Monoid (MatchPredicate backend) =>
      FilterBackend backend where

    -- | The part of object which we are filtering on,
    -- is provided by server backend implementor.
    type AutoFilteredValue backend a

    -- | A resulting predicate.
    -- Should have a 'Monoid' instance which forms a predicates conjunction.
    data MatchPredicate backend

-- | Implementation of auto filter we provide.
type AutoFilterImpl backend a =
    AutoFilteredValue backend a -> MatchPredicate backend

-- | How to apply a filter - what server backend implementor provides.
data FilteringApp backend param where
    AutoFilteringApp
        :: Typeable a
        => AutoFilteredValue backend a
        -> FilteringApp backend ('TyNamedParam name ('AutoFilter a))
    ManualFilteringApp
        :: Typeable a
        => (a -> MatchPredicate backend)
        -> FilteringApp backend ('TyNamedParam name ('ManualFilter a))

-- | Apply filter, evaluate whether a value matches or not.
backendApplyFilter
    :: TypeAutoFiltersSupport backend a
    => FilteringApp backend ('TyNamedParam name (fk a))
    -> TypeFilter fk a
    -> MatchPredicate backend
backendApplyFilter (AutoFilteringApp field) (TypeAutoFilter filtr) =
    typeAutoFiltersSupport filtr field
backendApplyFilter (ManualFilteringApp app) (TypeManualFilter val) =
    app val

-- | Implementation of given auto filter type for Beam Postgres backend.
class (Typeable filter, FilterBackend backend) =>
      AutoFilterSupport backend filter a where
    -- | Apply given filter to a value.
    autoFilterSupport :: filter a -> AutoFilterImpl backend a

-- | Enlists a way to apply each of supported filters at target application backend.
type FilteringSpecApp backend params =
    HList (FilteringApp backend) params

-------------------------------------------------------------------------
-- Implementation
-------------------------------------------------------------------------

-- | Force a type family to be defined.
-- Primarily for prettier error messages.
type family AreFiltersDefined (a :: [* -> *]) :: Constraint where
    AreFiltersDefined '[] = Show (Int -> Int)
    AreFiltersDefined a = ()

-- | Lookup among filters supported for this type and prepare
-- an appropriate one for (deferred) application.
class TypeAutoFiltersSupport' backend (filters :: [* -> *]) a where
    typeAutoFiltersSupport' :: SomeTypeAutoFilter a -> Maybe (AutoFilterImpl backend a)

instance TypeAutoFiltersSupport' backend '[] a where
    typeAutoFiltersSupport' _ = Nothing

instance ( AutoFilterSupport backend filter a
         , TypeAutoFiltersSupport' backend filters a
         , Typeable a
         ) =>
         TypeAutoFiltersSupport' backend (filter ': filters) a where
    typeAutoFiltersSupport' sf@(SomeTypeAutoFilter filtr) = asum
        [ do
          Identity filter' <- gcast1 @_ @_ @filter (Identity filtr)
          return $ autoFilterSupport filter'

        , typeAutoFiltersSupport' @backend @filters sf
        ]

type TypeAutoFiltersSupport backend a =
    ( AreFiltersDefined (SupportedFilters a)
    , TypeAutoFiltersSupport' backend (SupportedFilters a) a
    )

-- | Safely choose an appropriate filter from supported ones
-- and prepare it for application.
typeAutoFiltersSupport
    :: forall backend a.
       TypeAutoFiltersSupport backend a
    => SomeTypeAutoFilter a -> AutoFilterImpl backend a
typeAutoFiltersSupport filtr =
    typeAutoFiltersSupport' @backend @(SupportedFilters a) @a filtr
    ?: error "impossible, invariants of SomeTypeFilter are violated"

-- | Lookups for an appropriate filter application in a given 'FilteringSpecApp'
-- and applies it to a given filter.
class FilterBackend backend =>
      BackendApplySomeFilter backend (params :: [TyNamedFilter]) where
    backendApplySomeFilter'
        :: FilteringSpecApp backend params
        -> SomeFilter params
        -> Maybe (MatchPredicate backend)

instance FilterBackend backend =>
         BackendApplySomeFilter backend '[] where
    backendApplySomeFilter' _ _  = Nothing

instance ( Typeable fk, Typeable a
         , FilterBackend backend
         , TypeAutoFiltersSupport backend a
         , KnownSymbol name
         , BackendApplySomeFilter backend params
         ) =>
         BackendApplySomeFilter backend ('TyNamedParam name (fk a) ': params) where
    backendApplySomeFilter' (app `HCons` fields) (SomeFilter name filtr) = asum
        [ do
          guard (symbolValT @name == name)
          let filtr' :: TypeFilter fk a =
                cast filtr ?: error "Something is wrong, failed to cast filter!"
          return $ backendApplyFilter app filtr'

        , backendApplySomeFilter' @backend @params fields (SomeFilter name filtr)
        ]

-- | Applies a filter to a set of response fields which matter for filtering.
backendApplySomeFilter
    :: BackendApplySomeFilter backend params
    => FilteringSpecApp backend params
    -> SomeFilter params
    -> MatchPredicate backend
backendApplySomeFilter app filtr =
    backendApplySomeFilter' app filtr ?: error "SomeFilter invariants violated"
    -- TODO: actually we're not protected from this error as soon as SomeFilter can be
    -- unwrapped and wrapped back

-- | Applies multiple filters to a set of response fields which matter for filtering.
backendApplyFilters
    :: BackendApplySomeFilter backend params
    => FilteringSpec params
    -> FilteringSpecApp backend params
    -> MatchPredicate backend
backendApplyFilters (FilteringSpec filters) app =
    mconcat $ map (backendApplySomeFilter app) filters
