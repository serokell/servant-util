module Servant.Util.Beam.Postgres.Filtering
    ( fieldFilter_
    , applyFilters
    ) where

import Data.Typeable (cast, gcast1)
import Universum

import Database.Beam.Backend.SQL (HasSqlValueSyntax, IsSql92ExpressionSyntax,
                                  Sql92ExpressionValueSyntax)
import Database.Beam.Query (HasSqlEqualityCheck, in_, val_, (&&.), (/=.), (<.), (<=.), (==.), (>.),
                            (>=.))
import Database.Beam.Query.Internal (QExpr)

import Servant.Util.Common
import Servant.Util.Filtering

-- | Implementation of given filter type for Beam Postgres backend.
class Typeable filter =>
      FilterSupport syntax s filter a where
    -- | Apply given filter to a value.
    filterSupport :: filter a -> QExpr syntax s a -> QExpr syntax s Bool

instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a
         , HasSqlEqualityCheck syntax a
         ) =>
         FilterSupport syntax s FilterMatching a where
    filterSupport = \case
        FilterMatching v -> (==. val_ v)
        FilterNotMatching v -> (/=. val_ v)
        FilterItemsIn vs -> (`in_` map val_ vs)

instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a
         , IsSql92ExpressionSyntax syntax
         ) =>
         FilterSupport syntax s FilterComparing a where
    filterSupport = \case
        FilterGT v -> (>. val_ v)
        FilterLT v -> (<. val_ v)
        FilterGTE v -> (>=. val_ v)
        FilterLTE v -> (<=. val_ v)

-- TODO:
-- instance FilterSupport syntax s FilterOnLikeTemplate a where

-- | 'SomeTypeFilter' application.
class TypeFiltersSupport' syntax s (filters :: [* -> *]) a where
    typeFiltersSupport'
        :: SomeTypeFilter a
        -> QExpr syntax s a
        -> Maybe (QExpr syntax s Bool)

instance TypeFiltersSupport' syntax s '[] a where
    typeFiltersSupport' _ _ = Nothing

instance ( FilterSupport syntax s filter a
         , TypeFiltersSupport' syntax s filters a
         ) =>
         TypeFiltersSupport' syntax s (filter ': filters) a where
    typeFiltersSupport' sf@(SomeTypeFilter filtr) v = asum
        [ do
          Identity filter' <- gcast1 @_ @_ @filter (Identity filtr)
          return $ filterSupport filter' v

        , typeFiltersSupport' @syntax @s @filters sf v
        ]

type TypeFiltersSupport syntax s a =
    TypeFiltersSupport' syntax s (SupportedFilters a) a

-- | Safely applies some filter for given type to a value of this type.
typeFiltersSupport
    :: forall syntax s a.
       TypeFiltersSupport syntax s a
    => SomeTypeFilter a -> QExpr syntax s a -> QExpr syntax s Bool
typeFiltersSupport filtr v =
    typeFiltersSupport' @syntax @s @(SupportedFilters a) @a filtr v
    ?: error "impossible, invariants of SomeTypeFilter are violated"

-- | Some field participating in filtering.
newtype FilteredField syntax s (param :: TyNamedParam *) =
    FilteredField (QExpr syntax s (TyNamedParamType param))

{- | List of response fields we want to allow filtering on.

Example:

@
filteringSpecApp :: FilteringSpecApp ["course" ?: Course, "desc" ?: Text]
filteringSpecApp =
    fieldFilter_ @"course" courseField .*.
    fieldFilter_ @"desc" descField .*.
    HNil
@

Annotating 'fieldFilter' call with parameter name is fully optional and used only
to visually disambiguate filters of the same types.
-}
type FilteringSpecApp syntax s params =
    HList (FilteredField syntax s) params

-- | Applies some filter for some type to a given value, if their types match.
class ApplyFilter' syntax s params where
    applyFilter'
        :: FilteringSpecApp syntax s params
        -> SomeFilter params
        -> Maybe (QExpr syntax s Bool)

instance ApplyFilter' syntax s '[] where
    applyFilter' _ _  = Nothing

instance ( TypeFiltersSupport syntax s a
         , Typeable a
         , ApplyFilter' syntax s params
         ) =>
         ApplyFilter' syntax s ('TyNamedParam name a ': params) where
    applyFilter' (FilteredField field `HCons` fields) (SomeFilter filtr) = asum
        [ do
          filter' <- cast filtr
          return $ typeFiltersSupport filter' field

        , applyFilter' @syntax @s @params fields (SomeFilter filtr)
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

-- | Make a 'FilteredField'.
fieldFilter_
    :: forall name a syntax s.
       QExpr syntax s a -> FilteredField syntax s ('TyNamedParam name a)
fieldFilter_ = FilteredField
