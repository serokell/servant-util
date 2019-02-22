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
    , reifyParamsNamesSet
    , ParamsContainNoName
    , Elem
    , If
    , type (==)
    , type (&&)
    , type (||)
    , type (++)
    , type (//)
    , InsSorted
    , UnionSorted
    ) where

import Universum

import qualified Data.Set as S
import GHC.TypeLits (AppendSymbol, CmpSymbol, ErrorMessage (..), KnownSymbol, Symbol, TypeError,
                     symbolVal)
import Servant (If)

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
    reifyParamsNames' :: [Text]

instance ReifyParamsNames '[] where
    reifyParamsNames' = mempty

instance (KnownSymbol name, ReifyParamsNames params, ParamsContainNoName params name) =>
         ReifyParamsNames ('TyNamedParam name (p :: k) ': params) where
    reifyParamsNames' =
        toText (symbolVal @name Proxy) : reifyParamsNames' @k @params

reifyParamsNames :: forall params. ReifyParamsNames params => [Text]
reifyParamsNames = reifyParamsNames' @_ @params

-- | All names are quaranteed to be unique, so taking set of them is safe
-- (number of entires will be preserved).
reifyParamsNamesSet :: forall params. ReifyParamsNames params => Set Text
reifyParamsNamesSet = S.fromList $ reifyParamsNames @params

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
        TypeError ('Text "Duplicate name in parameters: " ':<>: 'ShowType name)
    ParamsContainNoName ('TyNamedParam name p ': params) name' =
        ParamsContainNoName params name'

-- Some of these can be removed in lts-12
-- (currently base has non-polykinded versions).

type family Elem (a :: k) (l :: [k]) :: Bool where
    Elem a '[]      = 'False
    Elem a (a ': l) = 'True
    Elem a (_ ': l) = Elem a l

type family (==) (a :: k) (b :: k) :: Bool where
    a == a = 'True
    _ == _ = 'False
infix 4 ==

type family (&&) (a :: Bool) (b :: Bool) :: Bool where
    'False && _ = 'False
    _ && 'False = 'False
    'True && 'True = 'True
infix 3 &&

type family (||) (a :: Bool) (b :: Bool) :: Bool where
    'True || _ = 'True
    _ || 'True = 'True
    'False || 'False = 'False
infix 2 ||

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
    '[] ++ bs = bs
    (a ': as) ++ bs = a ': (as ++ bs)

type family (//) (as :: [k]) (bs :: [k]) :: [k] where
    '[] // bs = '[]
    (a ': as) // bs = If (a `Elem` bs) (as // bs) (a ': (as // bs))

type family InsSorted (s :: Symbol) (ss :: [Symbol]) :: [Symbol] where
    InsSorted s '[] = '[s]
    InsSorted s0 (s ': ss) =
        If (CmpSymbol s0 s == 'GT) (s ': InsSorted s0 ss)
          ( If (s0 == s) (s ': ss) (s0 ': s ': ss)
          )

type family UnionSorted (ss1 :: [Symbol]) (ss2 :: [Symbol]) :: [Symbol] where
    UnionSorted ss1 '[] = ss1
    UnionSorted '[] ss2 = ss2
    UnionSorted (s1 ': ss1) (s2 ': ss2) =
        If (s1 == s2) (s1 ': UnionSorted ss1 ss2)
          ( If (CmpSymbol s1 s2 == 'LT)
               (s1 ': UnionSorted ss1 (s2 ': ss2))
               (s2 ': UnionSorted (s1 ': ss1) ss2)
          )
