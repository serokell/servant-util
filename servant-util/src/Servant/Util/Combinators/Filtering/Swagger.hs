{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Util.Combinators.Filtering.Swagger () where

import Universum

import Control.Lens ((<>~))
import qualified Data.HashMap.Strict.InsOrd as HM
import qualified Data.OpenApi as O
import qualified Data.Swagger as S
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol)
import Servant.API ((:>))
import Servant.OpenApi (HasOpenApi (..))
import Servant.Swagger (HasSwagger (..))

import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Common
import Servant.Util.Swagger

-- | Make a 'S.Param' for a filtering query parameter.
filterSwaggerParam :: forall a. S.ToParamSchema a => Text -> Text -> S.Param
filterSwaggerParam name desc =
    S.Param
    { S._paramName = name
    , S._paramDescription = Just desc
    , S._paramRequired = Just False
    , S._paramSchema = S.ParamOther S.ParamOtherSchema
        { S._paramOtherSchemaIn = S.ParamQuery
        , S._paramOtherSchemaAllowEmptyValue = Just True
        , S._paramOtherSchemaParamSchema = S.toParamSchema (Proxy @a)
        }
    }

-- | Make a 'O.Param' for a filtering query parameter.
filterOpenApiParam :: forall a. O.ToParamSchema a => Text -> Text -> O.Param
filterOpenApiParam name desc =
    O.Param
    { O._paramName = name
    , O._paramDescription = Just desc
    , O._paramRequired = Just False
    , O._paramDeprecated = Just False
    , O._paramIn = O.ParamQuery
    , O._paramAllowEmptyValue = Just True
    , O._paramAllowReserved = Nothing
    , O._paramStyle = Just O.StyleDeepObject
    , O._paramExample = Nothing
    , O._paramExamples = HM.empty
    , O._paramExplode = Just True
    , O._paramSchema = Just . O.Inline $ O.toParamSchema (Proxy @a)
    }


parenValueDesc :: forall a. KnownSymbol (ParamDescription a) => Text
parenValueDesc = "(" <> stripTrailingDot (symbolValT @(ParamDescription a)) <> ")"
  where
    stripTrailingDot t = T.stripSuffix "." t ?: t

autoFilterDesc :: forall a. KnownSymbol (ParamDescription a) => OpsDescriptions -> Text
autoFilterDesc ops = fullDesc
  where
    opsDesc
        | [(DefFilteringCmd, _)] <- ops = []
        | otherwise =
            "You can specify a custom filtering operation in `param[op]=value` \
            \or `param_op=value` format." :
            "Allowed operations:" :
            (ops <&> \(op, engDesc) -> "* `" <> op <> "` (" <> engDesc <> ")")

    noDefaultOpWarn =
        [ () | Nothing <- pure $ find ((DefFilteringCmd == ) . fst) ops ]
      *>
        [ ""
        , "_NB: Specifying filtering operation is mandatory for this parameter!_"
        ]

    fullDesc = unlines $
        [ "Filter values according to provided operation " <> parenValueDesc @a <> "."
        ] ++ opsDesc
          ++ noDefaultOpWarn

manualFilterDesc :: forall a. KnownSymbol (ParamDescription a) => Text
manualFilterDesc =
    "Leave values matching given parameter " <> parenValueDesc @a <> "."

-- | Gather swagger params for all of the given filters.
class AutoFiltersOpsDesc (filters :: [Type -> Type]) where
    autoFiltersOpsDesc :: OpsDescriptions

instance AutoFiltersOpsDesc '[] where
    autoFiltersOpsDesc = mempty

instance ( IsAutoFilter filter
         , AutoFiltersOpsDesc filters
         ) =>
         AutoFiltersOpsDesc (filter ': filters) where
    autoFiltersOpsDesc = mconcat
        [ autoFilterEnglishOpsNames @filter
        , autoFiltersOpsDesc @filters
        ]

-- | Get documentation for the given filter kind.
class FilterKindHasSwagger (fk :: FilterKind Type) where
    filterKindSwagger :: Text -> S.Param

instance DescribedSwaggerParam a => FilterKindHasSwagger ('ManualFilter a) where
    filterKindSwagger name = filterSwaggerParam @a name (manualFilterDesc @a)

instance (DescribedSwaggerParam a, AutoFiltersOpsDesc (SupportedFilters a)) =>
         FilterKindHasSwagger ('AutoFilter a) where
    filterKindSwagger name = filterSwaggerParam @a name (autoFilterDesc @a ops)
      where
        ops = autoFiltersOpsDesc @(SupportedFilters a)

-- | Get documentation for the given filter kind.
class FilterKindHasOpenApi (fk :: FilterKind Type) where
    filterKindOpenApi :: Text -> O.Param

instance DescribedOpenApiParam a => FilterKindHasOpenApi ('ManualFilter a) where
    filterKindOpenApi name = filterOpenApiParam @a name (manualFilterDesc @a)

instance (DescribedOpenApiParam a, AutoFiltersOpsDesc (SupportedFilters a)) =>
         FilterKindHasOpenApi ('AutoFilter a) where
    filterKindOpenApi name = filterOpenApiParam @a name (autoFilterDesc @a ops)
      where
        ops = autoFiltersOpsDesc @(SupportedFilters a)

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
        filterKindSwagger @fk (symbolValT @name) : filterParamsSwagger @params

instance (HasSwagger api, ReifyParamsNames params, FilterParamsHaveSwagger params) =>
         HasSwagger (FilteringParams params :> api) where
    toSwagger _ = toSwagger (Proxy @api)
        & S.allOperations . S.parameters <>~ map S.Inline (filterParamsSwagger @params)

-- | Get documentation for given filtering params.
class FilterParamsHaveOpenApi (params :: [TyNamedFilter]) where
    filterParamsOpenApi :: [O.Param]

instance FilterParamsHaveOpenApi '[] where
  filterParamsOpenApi = mempty

instance ( KnownSymbol name, FilterKindHasOpenApi fk
         , FilterParamsHaveOpenApi params
         ) =>
         FilterParamsHaveOpenApi ('TyNamedParam name fk ': params) where
    filterParamsOpenApi =
        filterKindOpenApi @fk (symbolValT @name) : filterParamsOpenApi @params

instance (HasOpenApi api, ReifyParamsNames params, FilterParamsHaveOpenApi params) =>
         HasOpenApi (FilteringParams params :> api) where
    toOpenApi _ = toOpenApi (Proxy @api)
        & O.allOperations . O.parameters <>~ map O.Inline (filterParamsOpenApi @params)
