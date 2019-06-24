{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Util.Combinators.Filtering.Swagger () where

import           Universum

import           Control.Lens                            ((<>~))
import           Data.Kind                               (Type)
import qualified Data.Swagger                            as S
import qualified Data.Text                               as T
import           GHC.TypeLits                            (KnownSymbol)
import           Servant.API                             ((:>))
import           Servant.Swagger                         (HasSwagger (..))

import           Servant.Util.Combinators.Filtering.Base
import           Servant.Util.Common
import           Servant.Util.Swagger

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

instance DescribedParam a => FilterKindHasSwagger ('ManualFilter a) where
    filterKindSwagger name = filterSwaggerParam @a name (manualFilterDesc @a)

instance (DescribedParam a, AutoFiltersOpsDesc (SupportedFilters a)) =>
         FilterKindHasSwagger ('AutoFilter a) where
    filterKindSwagger name = filterSwaggerParam @a name (autoFilterDesc @a ops)
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
