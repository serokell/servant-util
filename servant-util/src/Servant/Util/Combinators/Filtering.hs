-- | Allows complex filtering on specified fields.
module Servant.Util.Combinators.Filtering
    ( -- * General
      FilterKind (..)
    , FilteringParams
    , SupportedFilters
    , FilteringSpec (..)

      -- * Shortcuts
    , FilteringParamTypesOf
    , FilteringParamsOf
    , FilteringSpecOf

      -- * Filter types
    , FilterMatching (..)
    , FilterComparing (..)
    , FilterLike (..)
    , LikePattern (..)

    , NumericFilterTypes
    , TextFilterTypes
    , DatetimeFilterTypes

      -- * Manual construction
    , mkFilteringSpec
    , noFilters
    , ($=)
    , ($~)
    ) where

import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Combinators.Filtering.Client ()
import Servant.Util.Combinators.Filtering.Construction
import Servant.Util.Combinators.Filtering.Filters
import Servant.Util.Combinators.Filtering.Logging ()
import Servant.Util.Combinators.Filtering.Server ()
import Servant.Util.Combinators.Filtering.Support
