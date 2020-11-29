module Servant.Util.Combinators.ErrorResponses
    ( ErrorDesc (..)
    , ErrorPartialDesc (..)
    , ErrorResponses
    , type (#:)
    , type ($)
    , ExceptionalResponses
    ) where

import Universum

import Control.Lens (at, (<>~), (?~))
import qualified Data.Swagger as S
import Data.Swagger.Declare (runDeclare)
import GHC.TypeLits (KnownSymbol, Symbol)
import Servant ((:>), HasServer (..))
import Servant.Client (HasClient (..))
import Servant.Swagger (HasSwagger (..))

import Servant.Util.Combinators.Logging
import Servant.Util.Common


-- | Type-level information about an error response.
data ErrorDesc = ErrorDesc
    { erCode      :: Nat
    , erException :: Type
    , erDesc      :: Symbol
    }

-- | Like 'ErrorDesc', but without exception type yet.
data ErrorPartialDesc = ErrorPartialDesc
    { epdCode :: Nat
    , epdDesc :: Symbol
    }

{- | This combinator adds description of error response to swagger specification.

You have two ways to define this combinator:

* General:

@
ErrorResponses
   '[ 404 #! MyBackendException $
        "Not found"
    , 403 #! Int $
        "Operation is not permitted"
    ]
@

* When only one exception type is used:

@
ExceptionalResponses MyBackendException
   '[ 404 #: "Not found"
    , 403 #: "Operation is not permitted"
    ]
@

Note that if an error code was already defined further in endpoint definition,
it will be overwriten. For instance, 'Capture's define 400 error code (invalid
format); but in endpoint like @ErrorResponses (400 ...) :> Capture ... :> ...@
description for 400-error case provided by 'Capture' will be overwritten.

This combinator is transparent for server implementation.
-}
data ErrorResponses (errs :: [ErrorDesc])

type family ExceptionDesc err (codes :: [ErrorPartialDesc]) :: [ErrorDesc] where
    ExceptionDesc e '[] = '[]
    ExceptionDesc e ('ErrorPartialDesc code desc ': cs) =
        'ErrorDesc code e desc ': ExceptionDesc e cs

instance HasServer subApi ctx => HasServer (ErrorResponses errors :> subApi) ctx where
    type ServerT (ErrorResponses errors :> subApi) m = ServerT subApi m
    route _ = route (Proxy @subApi)
    hoistServerWithContext _ = hoistServerWithContext (Proxy @subApi)

instance HasClient m subApi => HasClient m (ErrorResponses errors :> subApi) where
    type Client m (ErrorResponses errors :> subApi) = Client m subApi
    clientWithRoute pm _ = clientWithRoute pm (Proxy @subApi)
    hoistClientMonad pm _ hst = hoistClientMonad pm (Proxy @subApi) hst

instance HasLoggingServer config subApi ctx =>
         HasLoggingServer config (ErrorResponses errors :> subApi) ctx where
    routeWithLog = inRouteServer @(ErrorResponses errors :> LoggingApiRec config subApi) route id


class KnownErrorCodes (errors :: [ErrorDesc]) where
    errorCodesToSwagger :: S.Swagger -> S.Swagger

instance KnownErrorCodes '[] where
    errorCodesToSwagger = id

instance (KnownNat code, KnownSymbol desc, S.ToSchema exc, KnownErrorCodes es) =>
         KnownErrorCodes ('ErrorDesc code exc desc ': es) where
    errorCodesToSwagger swagger = swagger
        & S.allOperations . S.responses . S.responses . at code ?~ S.Inline response
        & S.definitions <>~ defs
        & errorCodesToSwagger @es
      where
        code = fromIntegral (natVal @code Proxy)
        desc = symbolValT @desc
        (defs, excSchema) = runDeclare (S.declareSchemaRef (Proxy @exc)) mempty
        response = mempty
            & S.description .~ desc
            & S.schema ?~ excSchema

instance ( HasSwagger subApi
         , KnownErrorCodes errors
         ) => HasSwagger (ErrorResponses errors :> subApi) where
    toSwagger _ = toSwagger (Proxy @subApi) & errorCodesToSwagger @errors

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

-- | A convenient alias for use with 'ErrorResponse'.
type (#!) = 'ErrorDesc

-- | An alias for 'ErrorResponse' which allows to mention an exception type
-- just once across all errors specification.
type ExceptionalResponses err codes = ErrorResponses $ ExceptionDesc err codes

-- | A convenient alias for use with 'ExceptionalResponse'.
type (#:) = 'ErrorPartialDesc

---------------------------------------------------------------------------
-- Test samples
---------------------------------------------------------------------------

data MyBackendException

type Sample1 = ExceptionalResponses MyBackendException
   '[ 404 #: "Not found"
    , 403 #: "Operation is not permitted"
    ]

_sample1 :: Sample1
_sample1 = error "Just checked that kind of Sample1 is *"

type Sample2 =
    ErrorResponses
    '[ 404 #! MyBackendException $
         "Not found"
     , 403 #! Int $
         "Operation is not permitted"
     ]

_sample2 :: Sample2
_sample2 = error "Just checked that kind of Sample2 is *"
