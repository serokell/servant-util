{-# LANGUAGE PolyKinds #-}

module Servant.Util.Common.HList where

import Data.Kind (Type)


-- TODO: move to vinyl one day.

-- | Servant package defines their own 'HList', so we also can (to avoid a large dependency).
data HList (f :: k -> Type) (l :: [k]) where
    HNil :: HList f '[]
    HCons :: f a -> HList f as -> HList f (a ': as)
infixr 3 `HCons`

(.*.) :: forall k (f :: k -> Type) (a :: k) (as :: [k]).
         f a -> HList f as -> HList f (a : as)
(.*.) = HCons
infixr 3 .*.


class HListFromTuple a where
    type HListTuple a :: Type
    htuple :: HListTuple a -> a

instance HListFromTuple (HList f '[]) where
    type HListTuple (HList f '[]) = ()
    htuple () = HNil

instance HListFromTuple (HList f '[a]) where
    type HListTuple (HList f '[a]) = f a
    htuple a = a .*. HNil

instance HListFromTuple (HList f [a, b]) where
    type HListTuple (HList f [a, b]) = (f a, f b)
    htuple (a, b) = a .*. b .*. HNil

instance HListFromTuple (HList f [a, b, c]) where
    type HListTuple (HList f [a, b, c]) = (f a, f b, f c)
    htuple (a, b, c) = a .*. b .*. c .*. HNil

instance HListFromTuple (HList f [a, b, c, d]) where
    type HListTuple (HList f [a, b, c, d]) = (f a, f b, f c, f d)
    htuple (a, b, c, d) = a .*. b .*. c .*. d .*. HNil

instance HListFromTuple (HList f [a, b, c, d, e]) where
    type HListTuple (HList f [a, b, c, d, e]) = (f a, f b, f c, f d, f e)
    htuple (a, b, c, d, e) = a .*. b .*. c .*. d .*. e .*. HNil

instance HListFromTuple (HList f [a, b, c, d, e, g]) where
    type HListTuple (HList f [a, b, c, d, e, g]) = (f a, f b, f c, f d, f e, f g)
    htuple (a, b, c, d, e, g) = a .*. b .*. c .*. d .*. e .*. g .*. HNil

instance HListFromTuple (HList f [a, b, c, d, e, g, h]) where
    type HListTuple (HList f [a, b, c, d, e, g, h]) =
        (f a, f b, f c, f d, f e, f g, f h)
    htuple (a, b, c, d, e, g, h) =
        a .*. b .*. c .*. d .*. e .*. g .*. h .*. HNil

instance HListFromTuple (HList f [a, b, c, d, e, g, h, i]) where
    type HListTuple (HList f [a, b, c, d, e, g, h, i]) =
        (f a, f b, f c, f d, f e, f g, f h, f i)
    htuple (a, b, c, d, e, g, h, i) =
        a .*. b .*. c .*. d .*. e .*. g .*. h .*. i .*. HNil
