{-# LANGUAGE PolyKinds #-}

-- | Everything which requires "PolyKinded" extension, extracted in order
-- not to spoil the remaining code.
module Servant.Util.Common.PolyKinds
    ( TyNamedParam (..)
    , type (?:)
    , TyNamedParamType
    , ReifyParamsNames (..)
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

-- | Extract info from 'SortingParams'.
class ReifyParamsNames (params :: [TyNamedParam *]) where
    -- | Get all expected parameter names.
    reifyParamsNames :: Set Text

instance ReifyParamsNames '[] where
    reifyParamsNames = mempty

instance (KnownSymbol name, ReifyParamsNames params, ParamsContainNoName params name) =>
         ReifyParamsNames ('TyNamedParam name p ': params) where
    reifyParamsNames =
        toText (symbolVal @name Proxy) `S.insert` reifyParamsNames @params

type family ParamsContainNoName (params :: [TyNamedParam *]) name :: Constraint where
    ParamsContainNoName '[] name = ()
    ParamsContainNoName ('TyNamedParam name p ': params) name =
        TypeError ('Text "Duplicate name in sorting parameters " ':$$: 'ShowType name)
    ParamsContainNoName ('TyNamedParam name p ': params) name' =
        ParamsContainNoName params name'
