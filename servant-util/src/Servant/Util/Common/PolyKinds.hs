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
    , LookupParam
    , Elem
    ) where

import qualified Data.Set as S
import Universum

import GHC.TypeLits (AppendSymbol, ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)

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
    reifyParamsNames' :: Set Text

instance ReifyParamsNames '[] where
    reifyParamsNames' = mempty

instance (KnownSymbol name, ReifyParamsNames params, ParamsContainNoName params name) =>
         ReifyParamsNames ('TyNamedParam name (p :: k) ': params) where
    reifyParamsNames' =
        toText (symbolVal @name Proxy) `S.insert` reifyParamsNames' @k @params

reifyParamsNames :: forall params. ReifyParamsNames params => Set Text
reifyParamsNames = reifyParamsNames' @_ @params

type family LookupParam (desc :: Symbol) (name :: Symbol)
                        (params :: [TyNamedParam k]) :: k where
    LookupParam desc name '[] =
        TypeError ('Text ("No " `AppendSymbol` desc `AppendSymbol` " with name ")
                   ':<>: 'ShowType name ':<>: 'Text " found")
    LookupParam desc name ('TyNamedParam name a ': params) = a
    LookupParam desc name ('TyNamedParam name0 a ': params) =
        LookupParam desc name params

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
