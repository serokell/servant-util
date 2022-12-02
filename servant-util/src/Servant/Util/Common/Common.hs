module Servant.Util.Common.Common
    ( ApplicationLS
    , ApplicationRS
    , ApiHasArgClass (..)
    , ApiHasArg

    , inRouteServer
    , symbolValT

    , NameLabel (..)
    ) where

import Universum

import Fmt (build, pretty)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant.API (Capture, QueryFlag, QueryParam', ReqBody, (:>))
import Servant.API.Modifiers (RequiredArgument)
import Servant.Server (Handler (..), HasServer (..), Server)
import qualified Servant.Server.Internal as SI

type family ApplicationLS x where
    ApplicationLS (a b) = a

-- | Extract right side of type application.
type family ApplicationRS x where
    ApplicationRS (a b) = b

-- | Proves info about argument specifier of servant API.
class ApiHasArgClass api where
    -- | For arguments-specifiers of API, get argument type.
    -- E.g. @Capture "cap" Int@ -> @Int@.
    type ApiArg api :: Type
    type ApiArg api = ApplicationRS api

    -- | Name of argument.
    -- E.g. name of argument specified by @Capture "nyan"@ is /nyan/.
    apiArgName
        :: Proxy api -> String
    default apiArgName
        :: forall n someApiType a. (KnownSymbol n, api ~ someApiType n a)
        => Proxy api -> String
    apiArgName _ =
        pretty $ "'" <> build (symbolVal $ Proxy @n) <> "' field"

class ServerT (subApi :> res) m ~ (ApiArg subApi -> ServerT res m)
   => ApiHasArgInvariant subApi res m
instance ServerT (subApi :> res) m ~ (ApiArg subApi -> ServerT res m)
      => ApiHasArgInvariant subApi res m

type ApiHasArg subApi res =
    ( ApiHasArgClass subApi
    , ApiHasArgInvariant subApi res Handler
    )

instance KnownSymbol s => ApiHasArgClass (Capture s a)
instance KnownSymbol s => ApiHasArgClass (QueryParam' mods s a) where
    type ApiArg (QueryParam' mods s a) = RequiredArgument mods a
instance KnownSymbol s => ApiHasArgClass (QueryFlag s) where
    type ApiArg (QueryFlag s) = Bool
    apiArgName _ =
        pretty $ "'" <> build (symbolVal (Proxy @s)) <>  "' flag"
instance ApiHasArgClass (ReqBody ct a) where
    apiArgName _ = "request body"

-- | Modify handler in implementation of 'route'.
inRouteServer
    :: forall api api' ctx env.
       (Proxy api -> SI.Context ctx -> SI.Delayed env (Server api) -> SI.Router env)
    -> (Server api' -> Server api)
    -> (Proxy api' -> SI.Context ctx -> SI.Delayed env (Server api') -> SI.Router env)
inRouteServer routing f = \_ ctx delayed -> routing Proxy ctx (fmap f delayed)

-- | Similar to 'symbolVal', but shorter in use.
symbolValT :: forall s. KnownSymbol s => Text
symbolValT = fromString $ symbolVal (Proxy @s)

-- | Helper for passing type-level symbol at term-level.
-- We do not use 'Proxy' for this because defining
-- @instance IsLabel name (Proxy name)@ in a library is not a really good idea.
data NameLabel (name :: Symbol) = NameLabel

instance (n1 ~ n2) => IsLabel n1 (NameLabel n2) where
    fromLabel = NameLabel
