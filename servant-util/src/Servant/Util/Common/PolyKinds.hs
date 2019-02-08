{-# LANGUAGE PolyKinds #-}

-- | Everything which requires "PolyKinded" extension, extracted in order
-- not to spoil the remaining code.
module Servant.Util.Common.PolyKinds
    ( TyNamedParam (..)
    , type (?:)
    , TyNamedParamType
    , TyNamedParamsNames
    , ReifyParamsNames (..)
    , reifyParamsNames
    , Elem
    ) where

import qualified Data.Set as S
import Universum

import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)

-- | Pair of type and its name as it appears in API.
data TyNamedParam a = TyNamedParam Symbol a

-- | Convenient type alias for 'TyNamedParam'.
type (?:) = 'TyNamedParam

type family TyNamedParamType p where
    TyNamedParamType ('TyNamedParam _ a) = a

type family TyNamedParamsNames (params :: [TyNamedParam k]) :: [Symbol] where
    TyNamedParamsNames '[] = '[]
    TyNamedParamsNames ('TyNamedParam name _ ': params) = name ': TyNamedParamsNames params

-- | Extract info from 'SortingParams'.
class ReifyParamsNames (params :: [TyNamedParam k]) where
    -- | Get all expected parameter names.
    reifyParamsNamesP :: Proxy params -> Set Text

instance ReifyParamsNames '[] where
    reifyParamsNamesP _ = mempty

instance (KnownSymbol name, ReifyParamsNames params, ParamsContainNoName params name) =>
         ReifyParamsNames ('TyNamedParam name p ': params) where
    reifyParamsNamesP p =
        toText (symbolVal @name Proxy) `S.insert` reifyParamsNamesP p

reifyParamsNames :: forall params. ReifyParamsNames params => Set Text
reifyParamsNames = reifyParamsNamesP (Proxy @params)

type family ParamsContainNoName (params :: [TyNamedParam k]) name :: Constraint where
    ParamsContainNoName '[] name = ()
    ParamsContainNoName ('TyNamedParam name p ': params) name =
        TypeError ('Text "Duplicate name in sorting parameters " ':$$: 'ShowType name)
    ParamsContainNoName ('TyNamedParam name p ': params) name' =
        ParamsContainNoName params name'

type family Elem (a :: k) (l :: [k]) :: Bool where
    Elem a '[]      = 'False
    Elem a (a ': l) = 'True
    Elem a (_ ': l) = Elem a l
