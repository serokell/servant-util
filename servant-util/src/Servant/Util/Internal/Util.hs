{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Util.Internal.Util
    ( IsNotZero
    , KnownPositive
    , positiveVal
    ) where

import Universum

import GHC.TypeLits (ErrorMessage (..), KnownNat, Nat, TypeError)
import Numeric.Positive (Positive)
import Servant (FromHttpApiData (..), ToHttpApiData (..))

type family IsNotZero (k :: Nat) :: Constraint where
    IsNotZero 0 = TypeError ('Text "Null is now allowed here")
    IsNotZero k = ()

type KnownPositive k = (KnownNat k, IsNotZero k)

positiveVal :: forall k. (KnownPositive k) => Positive
positiveVal = fromIntegral $ natVal @k Proxy

instance FromHttpApiData Positive where
    parseUrlPiece t =
        parseUrlPiece @Natural t >>= \case
            0 -> Left "Zero is not allowed"
            k -> Right (fromIntegral k)

instance ToHttpApiData Positive where
    toUrlPiece = toUrlPiece @Natural . fromIntegral
