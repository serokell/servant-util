{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Util.Combinators.Sorting.Swagger () where

import Universum

import Control.Lens ((<>~), (?~))
import qualified Data.Swagger as S
import qualified Data.Text as T
import Servant.API ((:>))
import Servant.Swagger (HasSwagger (..))

import Servant.Util.Combinators.Sorting.Base
import Servant.Util.Common


instance (HasSwagger api, ReifyParamsNames params) =>
         HasSwagger (SortingParams params :> api) where
    toSwagger _ = toSwagger (Proxy @api)
        & S.allOperations . S.parameters <>~ [S.Inline param]
      where
        param = mempty
            & S.name .~ "sortBy"
            & S.description ?~ T.unlines
                [ "Allows lexicographical sorting on fields."
                , "General format is one of:"
                , "  * `+field1,-field2`"
                , "  * `asc(field1),desc(field2)`"
                , ""
                , " Fields allowed for this endpoint: " <> allowedFieldsDesc
                ]
            & S.required ?~ False
            & S.schema .~ S.ParamOther (mempty
                & S.in_ .~ S.ParamQuery
                & S.paramSchema .~ paramSchema
                )
        paramSchema = mempty
            & S.type_ .~ S.SwaggerString
            & S.pattern ?~ "^" <> fieldPattern <> "(," <> fieldPattern <> ")*" <> "$"
        fieldPattern =
            "(" <> "[+-](" <> allowedFieldsPattern <> ")+" <> "|" <>
            "(asc|desc)\\((" <> allowedFieldsPattern <> ")+\\))"
        allowedFields = reifyParamsNames @params
        allowedFieldsDesc =
            T.intercalate ", " $ map ((<> "`") . ("`" <>)) (toList allowedFields)
        allowedFieldsPattern = T.intercalate "|" (toList allowedFields)
