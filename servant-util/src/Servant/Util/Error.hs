-- | Errors in servant.
module Servant.Util.Error
    ( SimpleJSON
    ) where

import Universum

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Reflection (Reifies (..), reflect)
import Servant (JSON)
import Servant.API.ContentTypes (Accept (..), MimeRender (..), MimeUnrender (..))

-- | Custom json marker which sends no human-unreadable decoding errors
-- but a given fixed one.
data SimpleJSON err

instance Accept (SimpleJSON err) where
    contentTypes _ = contentTypes (Proxy @JSON)
instance ToJSON a => MimeRender (SimpleJSON err) a where
    mimeRender _ = encode
instance (FromJSON a, Reifies err String) => MimeUnrender (SimpleJSON err) a where
    mimeUnrender _ =
        let errMsg = reflect (Proxy @err)
        in first (\_ -> errMsg) . eitherDecode
