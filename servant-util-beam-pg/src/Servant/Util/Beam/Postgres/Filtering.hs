{-# LANGUAGE TypeInType #-}

module Servant.Util.Beam.Postgres.Filtering
    ( filterOn_
    , manualFilter_
    , applyFilters
    ) where

import Data.Typeable (cast, gcast, gcast1)
import Universum

import Data.Kind (type (*))
import Database.Beam.Backend.SQL (HasSqlValueSyntax, IsSql92ExpressionSyntax,
                                  Sql92ExpressionValueSyntax)
import Database.Beam.Query (HasSqlEqualityCheck, in_, val_, (&&.), (/=.), (<.), (<=.), (==.), (>.),
                            (>=.))
import Database.Beam.Query.Internal (QExpr)
import GHC.TypeLits (KnownSymbol)

import Servant.Util.Combinators.Filtering
import Servant.Util.Common


-- | Logic behind a filter which we can provide for user.
data BeamFilter syntax s a where
    AutoBeamFilter
        :: (QExpr syntax s a -> QExpr syntax s Bool)
        -> BeamFilter syntax s a
    ManualBeamFilter
        :: a -> BeamFilter syntax s a

-- | Backend implementation part - filter application.
data FilteringApp syntax s (param :: TyNamedFilter) where
    AutoFilteringApp
        :: Typeable a
        => QExpr syntax s a
        -> FilteringApp syntax s ('TyNamedParam name ('AutoFilter a))
    ManualFilteringApp
        :: Typeable a
        => (a -> QExpr syntax s Bool)
        -> FilteringApp syntax s ('TyNamedParam name ('ManualFilter a))

{- | List of response fields we want to allow filtering on.

Example:

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

Annotating 'fieldFilter' call with parameter name is fully optional and used only
to visually disambiguate filters of the same types.
-}
type FilteringSpecApp syntax s params =
    HList (FilteringApp syntax s) params


-- | Force a type family to be defined.
-- Primarily for prettier error messages.
type family AreFiltersDefined (a :: [* -> *]) :: Constraint where
    AreFiltersDefined '[] = Show ()
    AreFiltersDefined a = ()

-- | Implementation of given auto filter type for Beam Postgres backend.
class Typeable filter =>
      AutoFilterSupport syntax s filter a where
    -- | Apply given filter to a value.
    autoFilterSupport :: filter a -> QExpr syntax s a -> QExpr syntax s Bool

instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a
         , HasSqlEqualityCheck syntax a
         ) =>
         AutoFilterSupport syntax s FilterMatching a where
    autoFilterSupport = \case
        FilterMatching v -> (==. val_ v)
        FilterNotMatching v -> (/=. val_ v)
        FilterItemsIn vs -> (`in_` map val_ vs)

instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a
         , IsSql92ExpressionSyntax syntax
         ) =>
         AutoFilterSupport syntax s FilterComparing a where
    autoFilterSupport = \case
        FilterGT v -> (>. val_ v)
        FilterLT v -> (<. val_ v)
        FilterGTE v -> (>=. val_ v)
        FilterLTE v -> (<=. val_ v)

-- | Lookup among filters supported for this type and prepare
-- an appropriate one for (deferred) application.
class TypeFiltersSupport' syntax s (filters :: [* -> *]) a where
    typeFiltersSupport' :: SomeTypeFilter a -> Maybe (BeamFilter syntax s a)

instance TypeFiltersSupport' syntax s '[] a where
    typeFiltersSupport' _ = Nothing

instance ( AutoFilterSupport syntax s filter a  -- TODO: which is weird constraint
         , TypeFiltersSupport' syntax s filters a
         , Typeable a
         ) =>
         TypeFiltersSupport' syntax s (filter ': filters) a where
    typeFiltersSupport' sf = asum
        [ case sf of
            SomeTypeAutoFilter filtr -> do
              Identity filter' <- gcast1 @_ @_ @filter (Identity filtr)
              return $ AutoBeamFilter (autoFilterSupport filter')
            SomeTypeManualFilter val -> do
              val' <- cast val
              return $ ManualBeamFilter val'

        , typeFiltersSupport' @syntax @s @filters sf
        ]

type TypeFiltersSupport syntax s a =
    ( AreFiltersDefined (SupportedFilters a)
    , TypeFiltersSupport' syntax s (SupportedFilters a) a
    )

-- | Safely choose an appropriate filter from supported ones
-- and prepare it for application.
typeFiltersSupport
    :: forall syntax s a.
       TypeFiltersSupport syntax s a
    => SomeTypeFilter a -> BeamFilter syntax s a
typeFiltersSupport filtr =
    typeFiltersSupport' @syntax @s @(SupportedFilters a) @a filtr
    ?: error "impossible, invariants of SomeTypeFilter are violated"

-- | Merge a filter and previously found corresponding filtering application.
class ApplyAppropriateFilter' syntax s (fk :: * -> FilterKind *) a where
    applyAppropriateFilter'
        :: Typeable a
        => FilteringApp syntax s ('TyNamedParam name (fk a))
        -> SomeTypeFilter a
        -> QExpr syntax s Bool

instance TypeFiltersSupport syntax s a =>
         ApplyAppropriateFilter' syntax s 'AutoFilter a where
    applyAppropriateFilter' (AutoFilteringApp field) filtr =
        case typeFiltersSupport filtr of
            AutoBeamFilter app -> app field
            ManualBeamFilter{} -> error "Invalid filter kind"

instance TypeFiltersSupport syntax s a =>
         ApplyAppropriateFilter' syntax s 'ManualFilter a where
    applyAppropriateFilter' (ManualFilteringApp app) filtr =
        case typeFiltersSupport @syntax @s filtr of
            ManualBeamFilter v ->
                fmap app (cast v)
                ?: error "Something is wrong, failed to case value!"
            AutoBeamFilter{}   -> error "Invalid filter kind"

-- | Lookups for an appropriate filter application in a given 'FilteringSpecApp'
-- and applies it to a given filter.
class ApplyFilter' syntax s (params :: [TyNamedFilter]) where
    applyFilter'
        :: FilteringSpecApp syntax s params
        -> SomeFilter params
        -> Maybe (QExpr syntax s Bool)

instance ApplyFilter' syntax s '[] where
    applyFilter' _ _  = Nothing

instance ( Typeable a
         , ApplyAppropriateFilter' syntax s fk a
         , KnownSymbol name
         , ApplyFilter' syntax s params
         ) =>
         ApplyFilter' syntax s ('TyNamedParam name (fk a) ': params) where
    applyFilter' (app `HCons` fields) (SomeFilter name filtr) = asum
        [ do
          guard (symbolValT @name == name)
          let filtr' = gcast @_ @a filtr
                       ?: error "Something is wrong, failed to cast filter!"
          return $ applyAppropriateFilter' app filtr'

        , applyFilter' @syntax @s @params fields (SomeFilter name filtr)
        ]

-- | Applies a filter to a set of response fields which matter for filtering.
applyFilter
    :: ApplyFilter' syntax s params
    => FilteringSpecApp syntax s params
    -> SomeFilter params
    -> QExpr syntax s Bool
applyFilter app filtr =
    applyFilter' app filtr ?: error "SomeFilter invariants violated"
    -- TODO: actually we're not protected from this error as soon as SomeFilter can be
    -- unwrapped and wrapped back

-- | Applies a whole filtering specification to a set of response fields.
-- Resulting value can be put to 'guard_' or 'filter_' function.
applyFilters
    :: (ApplyFilter' syntax s params, IsSql92ExpressionSyntax syntax)
    => FilteringSpec params
    -> FilteringSpecApp syntax s params
    -> QExpr syntax s Bool
applyFilters (FilteringSpec filters) app =
    foldl' (\acc filtr -> acc &&. applyFilter app filtr) (val_ True) filters

-- | Implement an automatic filter.
-- User-provided filtering operation will do filter on this value.
filterOn_
    :: forall name a syntax s.
       Typeable a
    => QExpr syntax s a
    -> FilteringApp syntax s ('TyNamedParam name ('AutoFilter a))
filterOn_ = AutoFilteringApp

-- | Implement a manual filter.
-- You are provided with a value which user supplied and so you have
-- to construct a Beam predicate involving that value and relevant response fields.
manualFilter_
    :: forall name a syntax s.
       Typeable a
    => (a -> QExpr syntax s Bool)
    -> FilteringApp syntax s ('TyNamedParam name ('ManualFilter a))
manualFilter_ = ManualFilteringApp
