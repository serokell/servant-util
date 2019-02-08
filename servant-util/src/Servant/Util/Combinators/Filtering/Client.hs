{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Util.Combinators.Filtering.Client () where

import Universum

import Servant ((:>))
import Servant.Client (HasClient (..))

import Servant.Util.Combinators.Filtering.Base

-- | We do not yet support passing filtering parameters in client.
instance HasClient m subApi =>
         HasClient m (FilteringParams params :> subApi) where
    type Client m (FilteringParams params :> subApi) = Client m subApi
    clientWithRoute mp _ req = clientWithRoute mp (Proxy @subApi) req
