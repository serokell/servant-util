-- | Provides pagination API combinator.
module Servant.Util.Combinators.Pagination
    ( PaginationParams
    , PaginationPageSize (..)
    , KnownPaginationPageSize
    , PaginationSpec (..)
    , defPageSize
    , itemsOnPage
    , skipping
    , fullContent
    ) where

import Universum

import Control.Lens ((<>~), (?~))
import Data.Default (Default (..))
import qualified Data.OpenApi as O
import qualified Data.Swagger as S
import qualified Data.Text as T
import Servant (DefaultErrorFormatters, ErrorFormatters, HasContextEntry, HasServer (..),
                QueryParam, (:>))
import Servant.Client (HasClient (..))
import Servant.OpenApi (HasOpenApi (..))
import Servant.Swagger (HasSwagger (..))

import Servant.Server.Internal.Context (type (.++))
import Servant.Util.Combinators.Logging
import Servant.Util.Common
import Servant.Util.Internal.Util

-- | API combinator which enables pagination.
--
-- Pagination parameters are specified via @offset@ and @limit@ query parameters.
-- Both fields are optional; @offset@ defaults to @0@ and default value of @limit@
-- is defined in @settings@ argument.
--
-- Your endpoint implementation will be provided with 'PaginationSpec' variable
-- which will contain parameters provided by the user.
data PaginationParams (settings :: PaginationPageSize)

-- | Determines the page size used when client leaves it unspecified.
data PaginationPageSize
    -- | Use specified default.
    = DefPageSize Nat
    -- | Display all contents.
    | DefUnlimitedPageSize

-- | Contains pagination parameters provided by the user.
data PaginationSpec = PaginationSpec
    { psOffset :: Natural
      -- ^ How many elements to skip.
    , psLimit  :: Maybe (Positive Natural)
      -- ^ Maximum number of elements to leave.
      -- 'Nothing' stands for infinity.
    }

class KnownPaginationPageSize (settings :: PaginationPageSize) where
  settingDefPageSize :: Maybe (Positive Natural)

instance KnownPositive pageSize => KnownPaginationPageSize ('DefPageSize pageSize) where
  settingDefPageSize = Just (positiveVal @pageSize)

instance KnownPaginationPageSize 'DefUnlimitedPageSize where
  settingDefPageSize = Nothing

-- | How servant sees 'PaginationParams' under the hood.
type PaginationParamsExpanded subApi =
    QueryParam "offset" Natural :>
    QueryParam "limit" (Positive Natural) :>
    subApi

instance ( HasServer subApi ctx
         , HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters
         , KnownPaginationPageSize settings
         ) => HasServer (PaginationParams settings :> subApi) ctx where
    type ServerT (PaginationParams settings :> subApi) m =
        PaginationSpec -> ServerT subApi m

    route =
        inRouteServer @(PaginationParamsExpanded subApi) route $
        \handler offset limit ->
            handler PaginationSpec
            { psOffset = offset ?: 0
            , psLimit = limit <|> settingDefPageSize @settings
            }

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy @subApi) pc nt . s

instance
  ( HasLoggingServer config lcontext subApi ctx
  , KnownPaginationPageSize settings
  , HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  HasLoggingServer config lcontext (PaginationParams settings :> subApi) ctx where
    routeWithLog =
        inRouteServer @(PaginationParams settings :> LoggingApiRec config lcontext subApi) route $
        \(paramsInfo, handler) pagination@PaginationSpec{..} ->
            let text = merge . catMaybes $
                  [ guard (psOffset > 0) $> ("offset " <> show psOffset)
                  , fmap @Maybe
                      (\limit -> show (unPositive limit) <> " per page")
                      psLimit
                  ]
            in (addParamLogInfo text paramsInfo, handler pagination)
      where
        merge ts
            | null ts = "no pagination"
            | otherwise = T.intercalate ", " ts


-- | Do not paginate anything, use default page size.
defPageSize :: PaginationSpec
defPageSize = PaginationSpec{ psOffset = 0, psLimit = Nothing }

-- | Conveient builder for 'PaginationRequest', creates pagination
-- with zero offset and given limit.
itemsOnPage :: HasCallStack => Natural -> PaginationSpec
itemsOnPage limit = PaginationSpec
  { psOffset = 0
  , psLimit = Just (unsafeToPositive limit)
  }

-- | Convenient builder for 'PaginationRequest', modifies offset.
skipping :: Natural -> PaginationSpec -> PaginationSpec
skipping offset pagination = pagination{ psOffset = offset }

-- | Do not paginate anything.
fullContent :: PaginationSpec
fullContent = defPageSize
{-# DEPRECATED fullContent "Use `defPageSize` instead" #-}

-- | Retains full content.
instance Default PaginationSpec where
    def = defPageSize

instance HasClient m subApi =>
         HasClient m (PaginationParams settings :> subApi) where
    type Client m (PaginationParams settings :> subApi) =
        PaginationSpec -> Client m subApi

    clientWithRoute mp _ req PaginationSpec{..} =
        clientWithRoute mp (Proxy @(PaginationParamsExpanded subApi)) req
            (guard (psOffset > 0) $> psOffset)
            psLimit

    hoistClientMonad pm _ hst subCli = hoistClientMonad pm (Proxy @subApi) hst . subCli

-- Swagger instance
instance (HasSwagger api, KnownPaginationPageSize settings) =>
         HasSwagger (PaginationParams settings :> api) where
    toSwagger _ = toSwagger (Proxy @api)
        & S.allOperations . S.parameters <>~ [S.Inline offsetParam, S.Inline limitParam]
      where
        offsetParam :: S.Param
        limitParam :: S.Param
        offsetParam = mempty
            & S.name .~ "offset"
            & S.description ?~
                "Pagination parameter. How many items to skip from the beginning."
            & S.required ?~ False
            & S.schema .~ S.ParamOther (mempty
                & S.in_ .~ S.ParamQuery
                & S.paramSchema .~ offsetParamSchema
                )
        offsetParamSchema = mempty
            & S.type_ ?~ S.SwaggerInteger
            & S.format ?~ "int32"

        limitParam = mempty
            & S.name .~ "limit"
            & S.description ?~ mconcat
                [ "Pagination parameter. Maximum number of items to return.\n"
                , defaultPageSizeDesc
                ]
            & S.required ?~ False
            & S.schema .~ S.ParamOther (mempty
                & S.in_ .~ S.ParamQuery
                & S.paramSchema .~ limitParamSchema
                )
        limitParamSchema = mempty
            & S.type_ ?~ S.SwaggerInteger
            & S.format ?~ "int32"
            & S.pattern ?~ "^\\d*[1-9]\\d*$"
        defaultPageSizeDesc = case settingDefPageSize @settings of
          Nothing -> "By default, no limit will be applied."
          Just s  -> "Defaults to " <> show (unPositive s) <> "."

-- OpenApi instance
instance (HasOpenApi api, KnownPaginationPageSize settings) =>
         HasOpenApi (PaginationParams settings :> api) where
    toOpenApi _ = toOpenApi (Proxy @api)
        & O.allOperations . O.parameters <>~ [O.Inline offsetParam, O.Inline limitParam]
      where
        offsetParam :: O.Param
        limitParam :: O.Param
        offsetParam = mempty
            & O.name .~ "offset"
            & O.description ?~
                "Pagination parameter. How many items to skip from the beginning."
            & O.required ?~ False
            & O.in_ .~ O.ParamQuery
            & O.schema ?~ O.Inline offsetParamSchema
        offsetParamSchema = mempty
            & O.type_ ?~ O.OpenApiInteger
            & O.format ?~ "int32"

        limitParam = mempty
            & O.name .~ "limit"
            & O.description ?~ mconcat
                [ "Pagination parameter. Maximum number of items to return.\n"
                , defaultPageSizeDesc
                ]
            & O.required ?~ False
            & O.in_ .~ O.ParamQuery
            & O.schema ?~ O.Inline limitParamSchema
        limitParamSchema = mempty
            & O.type_ ?~ O.OpenApiInteger
            & O.format ?~ "int32"
            & O.pattern ?~ "^\\d*[1-9]\\d*$"
        defaultPageSizeDesc = case settingDefPageSize @settings of
          Nothing -> "By default, no limit will be applied."
          Just s  -> "Defaults to " <> show (unPositive s) <> "."
