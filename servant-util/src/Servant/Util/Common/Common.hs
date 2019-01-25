module Servant.Util.Common.Common
    ( ApplicationLS
    , ApplicationRS
    , ApiHasArgClass (..)
    , ApiHasArg

    , inRouteServer
    , symbolValT

    , TyNamedParam (..)
    , type (?:)
    , TyNamedParamType
    , ReifyParamsNames (..)
    ) where

import qualified Data.Set as S
import Universum

import qualified Data.Text.Buildable as B
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)
import Servant.API ((:>), Capture, QueryFlag, QueryParam, ReqBody)
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
    type ApiArg api :: *
    type ApiArg api = ApplicationRS api

    -- | Name of argument.
    -- E.g. name of argument specified by @Capture "nyan"@ is /nyan/.
    apiArgName
        :: Proxy api -> String
    default apiArgName
        :: forall n someApiType a. (KnownSymbol n, api ~ someApiType n a)
        => Proxy api -> String
    apiArgName _ =
        toString . pretty $ "'" <> B.build (symbolVal $ Proxy @n) <> "' field"

class ServerT (subApi :> res) m ~ (ApiArg subApi -> ServerT res m)
   => ApiHasArgInvariant subApi res m
instance ServerT (subApi :> res) m ~ (ApiArg subApi -> ServerT res m)
      => ApiHasArgInvariant subApi res m

type ApiHasArg subApi res =
    ( ApiHasArgClass subApi
    , ApiHasArgInvariant subApi res Handler
    )

instance KnownSymbol s => ApiHasArgClass (Capture s a)
instance KnownSymbol s => ApiHasArgClass (QueryParam s a) where
    type ApiArg (QueryParam s a) = Maybe a
instance KnownSymbol s => ApiHasArgClass (QueryFlag s) where
    type ApiArg (QueryFlag s) = Bool
    apiArgName _ =
        toString . pretty $ "'" <> B.build (symbolVal (Proxy @s)) <>  "' flag"
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

-- | Pair of type and its name as it appears in API.
data TyNamedParam a = TyNamedParam Symbol a

-- | Convenient type alias for 'TyNamedParam'.
type (?:) = 'TyNamedParam

type family TyNamedParamType p where
    TyNamedParamType ('TyNamedParam _ a) = a

-- | Extract info from 'SortingParams'.
class ReifyParamsNames (params :: [TyNamedParam *]) where
    -- | Get all expected parameter names.
    reifyParamsNames :: Set Text

instance ReifyParamsNames '[] where
    reifyParamsNames = mempty

instance (KnownSymbol name, ReifyParamsNames params, ParamsContainNoName params name) =>
         ReifyParamsNames ('TyNamedParam name p ': params) where
    reifyParamsNames =
        toText (symbolVal @name Proxy) `S.insert` reifyParamsNames @params

type family ParamsContainNoName (params :: [TyNamedParam *]) name :: Constraint where
    ParamsContainNoName '[] name = ()
    ParamsContainNoName ('TyNamedParam name p ': params) name =
        TypeError ('Text "Duplicate name in sorting parameters " ':$$: 'ShowType name)
    ParamsContainNoName ('TyNamedParam name p ': params) name' =
        ParamsContainNoName params name'
