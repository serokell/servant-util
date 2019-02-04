-- | Provides pagination API combinator.
module Servant.Util.Combinators.Pagination
    ( PaginationParams
    , PaginationSpec (..)
    , fullContent
    , itemsOnPage
    , skipping
    ) where

import Universum

import Data.Default (Default (..))
import qualified Data.Text as T
import Servant ((:>), HasServer (..), QueryParam)
import Servant.Client (HasClient (..))

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

instance HasServer subApi ctx => HasServer (PaginationParams :> subApi) ctx where
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

instance HasLoggingServer config subApi ctx =>
         HasLoggingServer config (PaginationParams :> subApi) ctx where
    routeWithLog =
        inRouteServer @(PaginationParams :> LoggingApiRec config subApi) route $
        \(paramsInfo, handler) pagination@PaginationSpec{..} ->
            let text = T.intercalate ", " . catMaybes $
                  [ guard (psOffset > 0) $> ("offset " <> show psOffset)
                  , fmap @Maybe
                      (\limit -> show (unPositive limit) <> " per page")
                      psLimit
                  ]
            in (addParamLogInfo text paramsInfo, handler pagination)

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
