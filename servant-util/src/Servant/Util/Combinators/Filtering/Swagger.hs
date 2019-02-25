{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Util.Combinators.Filtering.Swagger () where

import Universum

import Control.Lens ((<>~))
import Data.Kind (type (*))
import qualified Data.Map as M
import qualified Data.Swagger as S
import GHC.TypeLits (KnownSymbol)
import Servant.API ((:>))
import Servant.Swagger (HasSwagger (..))

import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Common
import Servant.Util.Swagger

data FilterKindDesc
    = AutoFilterDesc Text
    | ManualFilterDesc

-- | Make a 'S.Param' for a filtering query parameter.
filterSwaggerParam :: forall a. DescribedParam a => Text -> FilterKindDesc -> S.Param
filterSwaggerParam name filterDesc =
    S.Param
    { S._paramName = name
    , S._paramDescription = Just fullDesc
    , S._paramRequired = Just False
    , S._paramSchema = S.ParamOther S.ParamOtherSchema
        { S._paramOtherSchemaIn = S.ParamQuery
        , S._paramOtherSchemaAllowEmptyValue = Just True
        , S._paramOtherSchemaParamSchema = S.toParamSchema (Proxy @a)
        }
    }
  where
    valueDesc = symbolValT @(ParamDescription a)
    fullDesc = case filterDesc of
        AutoFilterDesc opDesc ->
            "Apply " <> show opDesc <> " filter to the given parameter (" <> valueDesc <> ")"
        ManualFilterDesc ->
            "Leave values matching given parameter (" <> valueDesc <> ")"

-- | Gather swagger params for all of the given filters.
class AutoFiltersHaveSwagger (filters :: [* -> *]) (a :: *) where
    autoFiltersSwagger :: Text -> [S.Param]

instance AutoFiltersHaveSwagger '[] a where
    autoFiltersSwagger _ = []

instance ( IsAutoFilter filter
         , DescribedParam a
         , AutoFiltersHaveSwagger filters a
         ) =>
         AutoFiltersHaveSwagger (filter ': filters) a where
    autoFiltersSwagger name =
        [ let op' = if op == defFilteringCmd then "" else "[" <> op <> "]"
              paramName = name <> op'
          in filterSwaggerParam @a paramName (AutoFilterDesc engDesc)
        | (op, engDesc) <- M.toList $ autoFilterEnglishOpsNames @filter
        ] ++ autoFiltersSwagger @filters @a name

-- | Get documentation for the given filter kind.
class FilterKindHasSwagger (fk :: FilterKind *) where
    filterKindSwagger :: Text -> [S.Param]

instance DescribedParam a => FilterKindHasSwagger ('ManualFilter a) where
    filterKindSwagger name = one $ filterSwaggerParam @a name ManualFilterDesc

instance AutoFiltersHaveSwagger (SupportedFilters a) a =>
         FilterKindHasSwagger ('AutoFilter a) where
    filterKindSwagger name = autoFiltersSwagger @(SupportedFilters a) @a name

-- | Get documentation for given filtering params.
class FilterParamsHaveSwagger (params :: [TyNamedFilter]) where
    filterParamsSwagger :: [S.Param]

instance FilterParamsHaveSwagger '[] where
    filterParamsSwagger = mempty

instance ( KnownSymbol name, FilterKindHasSwagger fk
         , FilterParamsHaveSwagger params
         ) =>
         FilterParamsHaveSwagger ('TyNamedParam name fk ': params) where
    filterParamsSwagger =
        filterKindSwagger @fk (symbolValT @name) <> filterParamsSwagger @params

instance (HasSwagger api, ReifyParamsNames params, FilterParamsHaveSwagger params) =>
         HasSwagger (FilteringParams params :> api) where
    toSwagger _ = toSwagger (Proxy @api)
        & S.allOperations . S.parameters <>~ map S.Inline (filterParamsSwagger @params)
