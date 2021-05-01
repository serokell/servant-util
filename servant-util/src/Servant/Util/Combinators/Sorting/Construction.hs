{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Manual construction of sorting spec.
module Servant.Util.Combinators.Sorting.Construction
    ( SortingRequestItem
    , asc, desc
    , mkSortingSpec
    , noSorting
    ) where

import Universum

import Data.Default (Default (..))
import GHC.Exts (IsList (..))
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError)

import Servant.Util.Combinators.Sorting.Base
import Servant.Util.Common


-- | Helper for defining custom 'SortingSpec's,
-- contains 'SortingItem' corresponding to one of parameter in @provided@ list.
newtype SortingRequestItem (provided :: [TyNamedParam *]) = SortingRequestItem
    { unSortingRequestItem :: SortingItem
    } deriving (Show)

type family KnownTypeName
    (origProvided :: [TyNamedParam *])
    (name :: Symbol)
    (provided :: [TyNamedParam *])
        :: Constraint where
    KnownTypeName orig name '[] =
        TypeError ('Text "Parameter " ':<>: 'ShowType name ':<>: 'Text " is not allowed here"
                   ':$$: 'Text "Available fields to sort on: " ':<>:
                         'ShowType (TyNamedParamsNames orig))
    KnownTypeName _ name ('TyNamedParam name _ ': _) = (KnownSymbol name)
    KnownTypeName orig name ('TyNamedParam name0 _ ': provided) = KnownTypeName orig name provided

-- | Ascendant sorting on a field with given name.
asc
    :: forall name provided.
       (KnownSymbol name, KnownTypeName provided name provided)
    => NameLabel name -> SortingRequestItem provided
asc _ = SortingRequestItem SortingItem
      { siName = symbolValT @name
      , siOrder = Ascendant
      }

-- | Ascendant sorting on a field with given name.
desc
    :: forall name provided.
       (KnownSymbol name, KnownTypeName provided name provided)
    => NameLabel name -> SortingRequestItem provided
desc _ = SortingRequestItem SortingItem
      { siName = symbolValT @name
      , siOrder = Descendant
      }

-- | Instance for 'SortingSpec' construction.
instance ReifySortingItems base => IsList (SortingSpec provided base) where
    type Item (SortingSpec provided base) = SortingRequestItem provided
    toList = map SortingRequestItem . ssProvided
    fromList = SortingSpec . map unSortingRequestItem

{- | Make a sorting specification.
Specified list should contain sorting on distinct fields; we do not enforce this
at type-level for convenience.

Example:

@
-- {-# LANGUAGE OverloadedLabels #-}

sortingSpec :: SortingSpec ["id" ?: Int, "desc" ?: Text]
sortingSpec = mkSortingSpec [asc #id]
@

-}
mkSortingSpec
    :: ReifySortingItems base
    => [SortingRequestItem provided] -> SortingSpec provided base
mkSortingSpec = fromList

-- | By default 'noSorting' is used.
instance ReifySortingItems base => Default (SortingSpec provided base) where
    def = noSorting

-- | Do not specify ordering.
noSorting :: ReifySortingItems base => SortingSpec provided base
noSorting = mkSortingSpec []
