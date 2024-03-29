module Servant.Util.Swagger
    ( ParamDescription
    , DescribedOpenApiParam
    , DescribedSwaggerParam
    , paramDescription

    , QueryFlagDescription

    , SwaggerrizeApi
    ) where

import Universum
import qualified Data.OpenApi.Lens as OT
import qualified Data.Swagger.Lens as ST

import Control.Exception (assert)
import Control.Lens (_head, ix, zoom, (?=))
import qualified Data.OpenApi as O
import qualified Data.Swagger as S
import GHC.TypeLits (KnownSymbol, Symbol)
import Servant (Capture', Description, EmptyAPI, NoContent, QueryFlag, QueryParam', Raw, StdMethod,
                Verb, (:<|>), (:>))
import Servant.OpenApi (HasOpenApi (..))
import Servant.Swagger (HasSwagger (..))

import Servant.Util.Common

----------------------------------------------------------------------------
-- Parameter description
----------------------------------------------------------------------------

-- | Description of parameter.
--
-- Unfortunately, @servant-swagger@ package, when deriving description of
-- an endpoint parameter, fills its description for you and makes you implement
-- just 'ParamSchema' which has no description field.
-- To circumvent that you can define description in instance of this type family
-- and later override swagger derivation accordingly.
type family ParamDescription a :: Symbol

type DescribedSwaggerParam a = (S.ToParamSchema a, KnownSymbol (ParamDescription a))

type DescribedOpenApiParam a = (O.ToParamSchema a, KnownSymbol (ParamDescription a))

-- | Set description according to 'ParamDescription' definition.
paramDescription
    :: forall a proxy.
       KnownSymbol (ParamDescription a)
    => proxy a -> Text
paramDescription _ = symbolValT @(ParamDescription a)

----------------------------------------------------------------------------
-- Capture description
----------------------------------------------------------------------------

-- | Like 'Capture', but does not modify description of 404 error (which looks
-- pretty like robot-generated).
-- See 'Servant.Util.Combinators.ErrorResponses' module for manual errors
-- definition.
data SwaggerCapture (mods :: [*]) (sym :: Symbol) a

instance (HasSwagger (Capture' mods sym a :> api), HasSwagger api) =>
         HasSwagger (SwaggerCapture mods sym a :> api) where
    toSwagger _ =
        toSwagger (Proxy @(Capture' mods sym a :> api))
            & desc404L .~ fromMaybe "" pureDesc404
      where
        desc404L :: Traversal' S.Swagger Text
        desc404L = S.allOperations . S.responses . S.responses .
                   ix 404 . ST._Inline . S.description
        pureDesc404 = toSwagger (Proxy @api) ^? desc404L

instance (HasOpenApi (Capture' mods sym a :> api), HasOpenApi  api) =>
         HasOpenApi  (SwaggerCapture mods sym a :> api) where
    toOpenApi _ =
        toOpenApi (Proxy @(Capture' mods sym a :> api))
            & desc404L .~ fromMaybe "" pureDesc404
      where
        desc404L :: Traversal' O.OpenApi Text
        desc404L = O.allOperations . O.responses . O.responses .
                   ix 404 . OT._Inline . O.description
        pureDesc404 = toOpenApi (Proxy @api) ^? desc404L

----------------------------------------------------------------------------
-- QueryParam description
----------------------------------------------------------------------------

-- | Like 'QueryParam', but does not modify description of 404 error.
-- See 'Servant.Util.Combinators.ErrorResponses' module for manual errors
-- definition.
data SwaggerQueryParam (mods :: [*]) (sym :: Symbol) a

instance (HasSwagger (QueryParam' mods sym a :> api), HasSwagger api) =>
         HasSwagger (SwaggerQueryParam mods sym a :> api) where
    toSwagger _ =
        toSwagger (Proxy @(QueryParam' mods sym a :> api))
            & desc404L .~ fromMaybe "" pureDesc404
      where
        desc404L :: Traversal' S.Swagger Text
        desc404L = S.allOperations . S.responses . S.responses .
                   ix 404 . ST._Inline . S.description
        pureDesc404 = toSwagger (Proxy @api) ^? desc404L

instance (HasOpenApi (QueryParam' mods sym a :> api), HasOpenApi api) =>
         HasOpenApi (SwaggerQueryParam mods sym a :> api) where
    toOpenApi _ =
      toOpenApi (Proxy @(QueryParam' mods sym a :> api))
          & desc404L .~ fromMaybe "" pureDesc404
      where
        desc404L :: Traversal' O.OpenApi Text
        desc404L = O.allOperations . O.responses . O.responses .
                   ix 404 . OT._Inline . O.description
        pureDesc404 = toOpenApi (Proxy @api) ^? desc404L

----------------------------------------------------------------------------
-- Query flag description
----------------------------------------------------------------------------

-- | Defines swagger description for the given `QueryFlag` parameter.
type family QueryFlagDescription (name :: Symbol) :: Symbol

-- | Replacement for 'QueryFlag' which has a better documentation.
data SwaggerQueryFlag (name :: Symbol)

type instance QueryFlagDescription "onlyCount" =
    "If this parameter is present, return only the total count of items."

instance (HasSwagger subApi, KnownSymbol name, KnownSymbol (QueryFlagDescription name)) =>
         HasSwagger (SwaggerQueryFlag name :> subApi) where
    toSwagger _ = toSwagger (Proxy @(QueryFlag name :> subApi)) `executingState` do
        zoom (S.allOperations . S.parameters . _head . ST._Inline) $ do
            paramName <- use S.name
            assert (name == paramName) pass
            S.description ?= desc
      where
        name = symbolValT @name
        desc = symbolValT @(QueryFlagDescription name)

instance (HasOpenApi subApi, KnownSymbol name, KnownSymbol (QueryFlagDescription name)) =>
         HasOpenApi (SwaggerQueryFlag name :> subApi) where
    toOpenApi _ = toOpenApi (Proxy @(QueryFlag name :> subApi)) `executingState` do
        zoom (O.allOperations . O.parameters . _head . OT._Inline) $ do
            paramName <- use O.name
            assert (name == paramName) pass
            O.description ?= desc
      where
        name = symbolValT @name
        desc = symbolValT @(QueryFlagDescription name)


----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

{- | This applies following transformations to API for the sake of better swagger
documentation.

* Response of methods returning `()` is replaced with `NoContents` (otherwise invalid
swagger is generated).

* `Capture`s and `QueryParam`s are attached a description according to
'ParamDescription' type family (default description is empty).

* @QueryFlag name@ occurences are attached descriptions according to
@ParamsDescription (QueryFlagDescription name)@ (there was no description by default).
-}
type family SwaggerrizeApi api where
    SwaggerrizeApi ((path :: Symbol) :> api) =
        path :> SwaggerrizeApi api

    SwaggerrizeApi (Capture' mods sym a :> api) =
        SwaggerCapture (Description (ParamDescription a) ': mods) sym a :> SwaggerrizeApi api

    SwaggerrizeApi (QueryParam' mods sym a :> api) =
        SwaggerQueryParam (Description (ParamDescription a) ': mods) sym a
        :> SwaggerrizeApi api

    SwaggerrizeApi (QueryFlag name :> api) =
        SwaggerQueryFlag name :> SwaggerrizeApi api

    SwaggerrizeApi (arg :> api) =
        arg :> SwaggerrizeApi api

    SwaggerrizeApi (api1 :<|> api2) =
        SwaggerrizeApi api1 :<|> SwaggerrizeApi api2

    SwaggerrizeApi (Verb (method :: StdMethod) (code :: Nat) ctx ()) =
        Verb method code ctx NoContent

    SwaggerrizeApi (Verb (method :: StdMethod) (code :: Nat) ctx a) =
        Verb method code ctx a

    SwaggerrizeApi Raw = Raw

    SwaggerrizeApi EmptyAPI = EmptyAPI
