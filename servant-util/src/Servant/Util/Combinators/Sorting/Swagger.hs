{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BlockArguments #-}

module Servant.Util.Combinators.Sorting.Swagger () where

import Universum

import Control.Lens ((<>~), (?~))
import qualified Data.Swagger as S
import qualified Data.Text as T
import Fmt (build, fmt, unlinesF)
import Servant.API ((:>))
import Servant.Swagger (HasSwagger (..))

import Servant.Util.Combinators.Sorting.Base
import Servant.Util.Common

instance (HasSwagger api, ReifySortingItems base, ReifyParamsNames provided) =>
         HasSwagger (SortingParams provided base :> api) where
    toSwagger _ = toSwagger (Proxy @api)
        & S.allOperations . S.parameters <>~ [S.Inline param]
      where
        param = mempty
            & S.name .~ "sortBy"
            & S.description ?~ fmt do
              unlinesF
                [ "Allows lexicographical sorting on fields."
                , "General format is one of:"
                , "  * `+field1,-field2`"
                , "  * `asc(field1),desc(field2)`"
                , ""
                , allowedFieldsDesc
                , baseFieldsDesc
                , " "
                ]
            & S.required ?~ False
            & S.schema .~ S.ParamOther (mempty
                & S.in_ .~ S.ParamQuery
                & S.paramSchema .~ paramSchema
                )
        paramSchema = mempty
            & S.type_ ?~ S.SwaggerString
            & S.pattern ?~ "^" <> fieldPattern <> "(," <> fieldPattern <> ")*" <> "$"
        fieldPattern =
            "(" <> "[+-](" <> allowedFieldsPattern <> ")+" <> "|" <>
            "(asc|desc)\\((" <> allowedFieldsPattern <> ")+\\))"
        allowedFields = reifyParamsNames @provided
        allowedFieldsDesc = mconcat
            [ " Fields allowed for this endpoint: "
            , mconcat . intersperse ", " $
                  map ((<> "`") . ("`" <>) . build) allowedFields
            ]
        allowedFieldsPattern = T.intercalate "|" allowedFields
        baseFields = reifySortingItems @base
        baseFieldsDesc
            | null baseFields = ""
            | otherwise = mconcat
                [ " Base sorting (always applied, lexicographically last): "
                , "`"
                , mconcat . intersperse ", " $ map build baseFields
                , "`"
                ]
