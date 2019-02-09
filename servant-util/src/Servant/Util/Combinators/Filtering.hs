-- | Allows complex filtering on specified fields.
module Servant.Util.Combinators.Filtering
    ( FilterKind (..)
    , TyNamedFilter
    , FilteringParams
    , SupportedFilters
    , IsAutoFilter (..)
    , SomeTypeAutoFilter (..)
    , TypeFilter (..)
    , SomeFilter (..)
    , FilteringSpec (..)
    , noFilters

     -- * Backends
    , FilterBackend (..)
    , AutoFilterSupport (..)
    , FilteringApp (..)
    , FilteringSpecApp
    , BackendApplySomeFilter
    , typeAutoFiltersSupport
    , backendApplyFilters

     -- * Filter types
    , FilterMatching (..)
    , FilterComparing (..)
    , FilterOnLikeTemplate (..)
    , LikeFormatter (..)

    , NumericFilterTypes
    , TextFilterTypes
    , DatetimeFilterTypes

    , FilteringParamTypesOf
    , FilteringParamsOf
    , FilteringSpecOf
    ) where

import Servant.Util.Combinators.Filtering.Backend
import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Combinators.Filtering.Client ()
import Servant.Util.Combinators.Filtering.Filters
import Servant.Util.Combinators.Filtering.Logging ()
import Servant.Util.Combinators.Filtering.Server ()
