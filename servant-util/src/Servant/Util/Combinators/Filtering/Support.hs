-- | Auto filters support for basic types.
module Servant.Util.Combinators.Filtering.Support
    ( NumericFilterTypes
    , TextFilterTypes
    , DatetimeFilterTypes
    ) where

import Universum

import Data.Time.Clock (UTCTime)

import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Combinators.Filtering.Filters


type NumericFilterTypes = [FilterMatching, FilterComparing]
type TextFilterTypes = [FilterMatching, FilterComparing, FilterLike]
type ByteStringFilterTypes = [FilterMatching, FilterComparing]
type DatetimeFilterTypes = '[FilterComparing]

type instance SupportedFilters Bool = '[FilterMatching]
type instance SupportedFilters Int = NumericFilterTypes
type instance SupportedFilters Text = TextFilterTypes
type instance SupportedFilters ByteString = ByteStringFilterTypes
type instance SupportedFilters UTCTime = DatetimeFilterTypes
