module Servant.Util.Combinators.Tag
    ( Tag
    ) where

import Universum

import Control.Lens (at, (?~))
import qualified Data.Swagger as S
import GHC.TypeLits (KnownSymbol, Symbol)
import Servant ((:>), HasServer (..))
import Servant.Client (HasClient (..))
import Servant.Swagger (HasSwagger (..))

import Servant.Util.Combinators.Logging
import Servant.Util.Common

-- | Attaches a tag to swagger documentation.
-- Server implementation remains intact.
data Tag (name :: Symbol)

instance HasServer subApi ctx => HasServer (Tag name :> subApi) ctx where
    type ServerT (Tag name :> subApi) m = ServerT subApi m
    route _ = route (Proxy @subApi)
    hoistServerWithContext _ = hoistServerWithContext (Proxy @subApi)

instance HasClient m subApi => HasClient m (Tag name :> subApi) where
    type Client m (Tag name :> subApi) = Client m subApi
    clientWithRoute pm _ = clientWithRoute pm (Proxy @subApi)

instance HasLoggingServer config subApi ctx =>
         HasLoggingServer config (Tag name :> subApi) ctx where
    routeWithLog = inRouteServer @(Tag name :> LoggingApiRec config subApi) route identity

instance (HasSwagger subApi, KnownSymbol name) =>
         HasSwagger (Tag name :> subApi) where
    toSwagger _ = toSwagger (Proxy @subApi)
        & S.allOperations . S.tags . at name ?~ ()
      where
        name = symbolValT @name
