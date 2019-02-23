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
import GHC.Exts (IsList, fromList)
import qualified GHC.Exts
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError)

import Servant.Util.Combinators.Sorting.Base
import Servant.Util.Common


-- | Helper for defining custom 'SortingSpec's,
-- contains 'SortingItem' corresponding to one of parameter in @params@.
newtype SortingRequestItem (params :: [TyNamedParam *]) = SortingRequestItem
    { unSortingRequestItem :: SortingItem
    } deriving (Show)

type family KnownTypeName
    (origParams :: [TyNamedParam *])
    (name :: Symbol)
    (params :: [TyNamedParam *])
        :: Constraint where
    KnownTypeName orig name '[] =
        TypeError ('Text "Parameter " ':<>: 'ShowType name ':<>: 'Text " is not allowed here"
                   ':$$: 'Text "Available fields to sort on: " ':<>:
                         'ShowType (TyNamedParamsNames orig))
    KnownTypeName _ name ('TyNamedParam name _ ': _) = (KnownSymbol name)
    KnownTypeName orig name ('TyNamedParam name0 _ ': params) = KnownTypeName orig name params

-- | Ascendant sorting on a field with given name.
asc
    :: forall name params.
       (KnownSymbol name, KnownTypeName params name params)
    => NameLabel name -> SortingRequestItem params
asc _ = SortingRequestItem SortingItem
      { siName = symbolValT @name
      , siOrder = Ascendant
      }

-- | Ascendant sorting on a field with given name.
desc
    :: forall name params.
       (KnownSymbol name, KnownTypeName params name params)
    => NameLabel name -> SortingRequestItem params
desc _ = SortingRequestItem SortingItem
      { siName = symbolValT @name
      , siOrder = Descendant
      }

instance IsList (SortingSpec params) where
    type Item (SortingSpec params) = SortingRequestItem params
    toList = map SortingRequestItem . unSortingSpec
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
mkSortingSpec :: [SortingRequestItem params] -> SortingSpec params
mkSortingSpec = fromList

-- | By default 'noSorting' is used.
instance Default (SortingSpec params) where
    def = noSorting

-- | Do not specify ordering.
noSorting :: SortingSpec params
noSorting = mkSortingSpec []
