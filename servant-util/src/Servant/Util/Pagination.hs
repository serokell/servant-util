-- | Provides pagination API combinator.
module Servant.Util.Pagination
    ( PaginationParams
    , PaginationSettings (..)
    , PaginationSpec (..)
    , itemsOnPage
    , skipping
    ) where

import Universum

import Data.Default (Default (..))
import qualified Data.Text as T
import GHC.TypeLits (Nat)
import Numeric.Positive (Positive)
import Servant ((:>), HasServer (..), QueryParam)
import Servant.Client (HasClient (..))

import Servant.Util.Common
import Servant.Util.Internal.Util
import Servant.Util.Logging

-- | Settings used to define default number of items per page.
data PaginationSettings
    = DefPageSize Nat

-- | API combinator which enables pagination.
--
-- Pagination parameters are specified via @offset@ and @limit@ query parameters.
-- Both fields are optional; @offset@ defaults to @0@ and default value of @limit@
-- is defined in @settings@ argument.
--
-- Your endpoint implementation will be provided with 'PaginationSpec' variable
-- which will contain parameters provided by the user.
data PaginationParams (settings :: PaginationSettings)

-- | Contains pagination parameters provided by the user.
-- 'psLimit' field cannot be limit.
data PaginationSpec = PaginationSpec
    { psOffset :: Natural
    , psLimit  :: Positive
    }

-- | How servant sees 'PaginationParams' under the hood.
type PaginationParamsExpanded subApi =
    QueryParam "offset" Natural :>
    QueryParam "limit" Positive :>
    subApi

instance ( HasServer subApi ctx
         , settings ~ 'DefPageSize defPageSize
         , KnownPositive defPageSize
         ) =>
         HasServer (PaginationParams settings :> subApi) ctx where
    type ServerT (PaginationParams settings :> subApi) m =
        PaginationSpec -> ServerT subApi m

    route =
        inRouteServer @(PaginationParamsExpanded subApi) route $
        \handler offset limit ->
            handler PaginationSpec
            { psOffset = offset ?: 0
            , psLimit = limit ?: positiveVal @defPageSize
            }

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy @subApi) pc nt . s

instance ( HasLoggingServer config subApi ctx
         , settings ~ 'DefPageSize defPageSize
         , KnownPositive defPageSize
         ) =>
         HasLoggingServer config (PaginationParams settings :> subApi) ctx where
    routeWithLog =
        inRouteServer @(PaginationParams settings :> LoggingApiRec config subApi) route $
        \(paramsInfo, handler) pagination@PaginationSpec{..} ->
            let text = T.intercalate ", "
                  [ if psOffset == 0
                    then ""
                    else "offset " <> show psOffset
                  , show psLimit <> " per page"
                  ]
            in (addParamLogInfo text paramsInfo, handler pagination)

-- | Conveient builder for 'PaginationRequest', creates pagination
-- with zero offset and given limit.
itemsOnPage :: Positive -> PaginationSpec
itemsOnPage limit = PaginationSpec{ psOffset = 0, psLimit = limit }

-- | Convenient builder for 'PaginationRequest', modifies offset.
skipping :: Natural -> PaginationSpec -> PaginationSpec
skipping offset pagination = pagination{ psOffset = offset }

-- | Retains full content.
instance Default PaginationSpec where
    def = itemsOnPage (fromIntegral $ maxBound @Word64)

instance HasClient m subApi =>
         HasClient m (PaginationParams settings :> subApi) where
    type Client m (PaginationParams settings :> subApi) =
        PaginationSpec -> Client m subApi

    clientWithRoute mp _ req PaginationSpec{..} =
        clientWithRoute mp (Proxy @(PaginationParamsExpanded subApi)) req
            (Just psOffset) (Just psLimit)
