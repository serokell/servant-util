{-# LANGUAGE PolyKinds #-}

module Servant.Util.Common.HList where

-- | Servant package defines their own 'HList', so we also can (to avoid a large dependency).
data HList (f :: k -> *) (l :: [k]) where
    HNil :: HList f '[]
    HCons :: f a -> HList f as -> HList f (a ': as)
infixr 3 `HCons`

(.*.) :: forall (f :: k -> *) (a :: k) (as :: [k]).
         f a -> HList f as -> HList f (a : as)
(.*.) = HCons
infixr 3 .*.
