-- | Allows complex filtering on specified fields.
module Servant.Util.Combinators.Filtering
    ( -- * General
      FilterKind (..)
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

    , filterOn
    , manualFilter

      -- * Filter types
    , FilterMatching (..)
    , FilterComparing (..)
    , FilterLike (..)
    , LikePattern (..)

    , NumericFilterTypes
    , TextFilterTypes
    , DatetimeFilterTypes

    , FilteringParamTypesOf
    , FilteringParamsOf
    , FilteringSpecOf

      -- * Client
    , mkFilteringSpec
    , ($=)
    , ($~)
    ) where

import Servant.Util.Combinators.Filtering.Backend
import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Combinators.Filtering.Client
import Servant.Util.Combinators.Filtering.Filters
import Servant.Util.Combinators.Filtering.Logging ()
import Servant.Util.Combinators.Filtering.Server ()
import Servant.Util.Combinators.Filtering.Support
