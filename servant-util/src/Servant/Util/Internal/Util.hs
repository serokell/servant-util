{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Util.Internal.Util
    ( Positive (..)
    , IsNotZero
    , KnownPositive
    , positiveVal
    ) where

import Universum

import GHC.TypeLits (ErrorMessage (..), TypeError)
import Servant (FromHttpApiData (..), ToHttpApiData (..))

newtype Positive a = PositiveUnsafe { unPositive :: a }
    deriving (Show, Eq, Ord)

toPositive :: (Show a, Ord a, Num a) => a -> Either Text (Positive a)
toPositive a
    | a > 0 = Right $ PositiveUnsafe a
    | otherwise = Left $ "Value is negative: " <> show a

type family IsNotZero (k :: Nat) :: Constraint where
    IsNotZero 0 = TypeError ('Text "Null is now allowed here")
    IsNotZero k = ()

type KnownPositive k = (KnownNat k, IsNotZero k)

positiveVal :: forall k i. (KnownPositive k, Num i) => Positive i
positiveVal = PositiveUnsafe . fromIntegral $ natVal @k Proxy

instance (FromHttpApiData a, Show a, Ord a, Num a) => FromHttpApiData (Positive a) where
    parseUrlPiece t = parseUrlPiece @a t >>= toPositive

instance ToHttpApiData a => ToHttpApiData (Positive a) where
    toUrlPiece = toUrlPiece @a . unPositive
