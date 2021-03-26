-- | Provides pagination API combinator.
module Servant.Util.Combinators.Pagination
    ( PaginationParams
    , PaginationSpec (..)
    , fullContent
    , itemsOnPage
    , skipping
    ) where

import Universum

import Control.Lens ((<>~), (?~))
import Data.Default (Default (..))
import qualified Data.Swagger as S
import qualified Data.Text as T
import Servant (DefaultErrorFormatters, ErrorFormatters, HasContextEntry, HasServer (..),
                QueryParam, (:>))
import Servant.Client (HasClient (..))
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
data PaginationParams

-- | Contains pagination parameters provided by the user.
-- 'psLimit' field cannot be limit.
data PaginationSpec = PaginationSpec
    { psOffset :: Natural
      -- ^ How many elements to skip.
    , psLimit  :: Maybe (Positive Natural)
      -- ^ Maximum number of elements to remain.
    }

-- | How servant sees 'PaginationParams' under the hood.
type PaginationParamsExpanded subApi =
    QueryParam "offset" Natural :>
    QueryParam "limit" (Positive Natural) :>
    subApi

instance ( HasServer subApi ctx
         , HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters
         ) => HasServer (PaginationParams :> subApi) ctx where
    type ServerT (PaginationParams :> subApi) m =
        PaginationSpec -> ServerT subApi m

    route =
        inRouteServer @(PaginationParamsExpanded subApi) route $
        \handler offset limit ->
            handler PaginationSpec
            { psOffset = offset ?: 0
            , psLimit = limit
            }

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy @subApi) pc nt . s

instance
  ( HasLoggingServer config subApi ctx
  , HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  HasLoggingServer config (PaginationParams :> subApi) ctx where
    routeWithLog =
        inRouteServer @(PaginationParams :> LoggingApiRec config subApi) route $
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

-- | Do not paginate anything.
fullContent :: PaginationSpec
fullContent = PaginationSpec{ psOffset = 0, psLimit = Nothing }

-- | Conveient builder for 'PaginationRequest', creates pagination
-- with zero offset and given limit.
itemsOnPage :: Positive Natural -> PaginationSpec
itemsOnPage limit = PaginationSpec{ psOffset = 0, psLimit = Just limit }

-- | Convenient builder for 'PaginationRequest', modifies offset.
skipping :: Natural -> PaginationSpec -> PaginationSpec
skipping offset pagination = pagination{ psOffset = offset }

-- | Retains full content.
instance Default PaginationSpec where
    def = fullContent

instance HasClient m subApi =>
         HasClient m (PaginationParams :> subApi) where
    type Client m (PaginationParams :> subApi) =
        PaginationSpec -> Client m subApi

    clientWithRoute mp _ req PaginationSpec{..} =
        clientWithRoute mp (Proxy @(PaginationParamsExpanded subApi)) req
            (guard (psOffset > 0) $> psOffset)
            psLimit

    hoistClientMonad pm _ hst subCli = hoistClientMonad pm (Proxy @subApi) hst . subCli

instance HasSwagger api => HasSwagger (PaginationParams :> api) where
    toSwagger _ = toSwagger (Proxy @api)
        & S.allOperations . S.parameters <>~ [S.Inline offsetParam, S.Inline limitParam]
      where
        offsetParam :: S.Param
        limitParam :: S.Param
        offsetParam = mempty
            & S.name .~ "offset"
            & S.description ?~ "Pagination parameter. How many items to skip from \
                               \the beginning."
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
            & S.description ?~ "Pagination parameter. Maximum number of items to return."
            & S.required ?~ False
            & S.schema .~ S.ParamOther (mempty
                & S.in_ .~ S.ParamQuery
                & S.paramSchema .~ limitParamSchema
                )
        limitParamSchema = mempty
            & S.type_ ?~ S.SwaggerInteger
            & S.format ?~ "int32"
