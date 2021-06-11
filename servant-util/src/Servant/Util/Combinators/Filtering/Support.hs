-- | Auto filters support for basic types.
module Servant.Util.Combinators.Filtering.Support
    ( NumericFilterTypes
    , TextFilterTypes
    , DatetimeFilterTypes
    , AllFilterTypes
    ) where

import Universum

import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime)

import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Combinators.Filtering.Filters


type NumericFilterTypes = [FilterMatching, FilterComparing]
type TextFilterTypes = [FilterMatching, FilterComparing, FilterLike]
type ByteStringFilterTypes = [FilterMatching, FilterComparing]
type DatetimeFilterTypes = '[FilterComparing]
type AllFilterTypes = '[FilterMatching, FilterComparing, FilterLike]

type instance SupportedFilters () = '[]

type instance SupportedFilters Bool = '[FilterMatching]

type instance SupportedFilters Integer = NumericFilterTypes
type instance SupportedFilters Int = NumericFilterTypes
type instance SupportedFilters Int8 = NumericFilterTypes
type instance SupportedFilters Int16 = NumericFilterTypes
type instance SupportedFilters Int32 = NumericFilterTypes
type instance SupportedFilters Int64 = NumericFilterTypes
type instance SupportedFilters Natural = NumericFilterTypes
type instance SupportedFilters Word = NumericFilterTypes
type instance SupportedFilters Word8 = NumericFilterTypes
type instance SupportedFilters Word16 = NumericFilterTypes
type instance SupportedFilters Word32 = NumericFilterTypes
type instance SupportedFilters Word64 = NumericFilterTypes
type instance SupportedFilters Float = NumericFilterTypes
type instance SupportedFilters Double = NumericFilterTypes

type instance SupportedFilters Char = [FilterMatching, FilterComparing]
type instance SupportedFilters Text = TextFilterTypes
type instance SupportedFilters ByteString = ByteStringFilterTypes

type instance SupportedFilters UTCTime = DatetimeFilterTypes
type instance SupportedFilters LocalTime = DatetimeFilterTypes
type instance SupportedFilters Day = NumericFilterTypes
