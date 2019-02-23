module Servant.Util.Combinators.Sorting.Base
    ( SortingParams
    , SortParamsExpanded
    , SortingSpec (..)
    , SortingOrder (..)
    , NullsSortingOrder (..)
    , SortingItemTagged (..)
    , SortingItem (..)
    , TaggedSortingItemsList
    , SortingParamTypesOf
    , SortingParamsOf
    , SortingSpecOf
    ) where

import Universum

import Fmt (Buildable (..))
import Servant ((:>), QueryParam)
import Servant.Server (Tagged (..))
import Servant.Util.Common

{- | Servant API combinator which allows to accept sorting parameters as a query parameter.

Example: with the following combinator

@
SortingParams ["time" ?: Timestamp, "name" ?: Text]
@

the endpoint can parse "sortBy=-time,+name" or "sortBy=desc(time),asc(name)" formats,
which would mean sorting by mentioned fields lexicographically. All sorting
subparameters are optional, as well as entire "sortBy" parameter.

Your handler will be provided with 'SortingSpec' argument which can later be passed
in an appropriate function to perform sorting.
-}
data SortingParams (allowed :: [TyNamedParam *])

-- | How servant sees 'SortParams' under the hood.
type SortParamsExpanded allowed subApi =
    QueryParam "sortBy" (TaggedSortingItemsList allowed) :> subApi

-- | Order of sorting.
data SortingOrder
    = Descendant
    | Ascendant
    deriving (Show, Eq, Enum)

-- | Where to place null fields.
data NullsSortingOrder
    = NullsFirst
    | NullsLast
    deriving (Show, Eq, Enum)

-- | For a given field, user-supplied order of sorting.
-- This type is primarly for internal use, see also 'SortingItemTagged'.
data SortingItem = SortingItem
    { siName  :: Text
      -- ^ Name of parameter.
      -- Always matches one in @param@, but we keep it at term-level as well for convenience.
    , siOrder :: SortingOrder
      -- ^ Sorting order on the given parameter.

    -- , siNullsOrder :: Maybe NullsSortingOrder
      ---- ^ Order of null fields.
      ---- Present only when the second element in @param@ tuple is 'Maybe'.
      ---- TODO [DSCP-425] add support for this
    } deriving (Show)

-- | Version 'SortingItem' which remembers its name and parameter type at type level.
-- In functions which belong to public API you will most probably want to use this datatype
-- as a safer variant of 'SortingItem'.
newtype SortingItemTagged (param :: TyNamedParam *) = SortingItemTagged
    { untagSortingItem :: SortingItem
    } deriving (Show)

instance Buildable SortingItem where
    build SortingItem{..} =
        let order = case siOrder of { Ascendant -> "⯅ "; Descendant -> "⯆ " }
        in order <> build siName

deriving instance Buildable (SortingItemTagged param)

-- | Tagged, because we want to retain list of allowed fields for parsing
-- (in @instance FromHttpApiData@).
type TaggedSortingItemsList allowed = Tagged (allowed :: [TyNamedParam *]) [SortingItem]

-- | What is passed to an endpoint, contains all sorting parameters provided by a user.
{- Following properties hold:
1. Each entry in the underlying list has a unique name ('siName' field).
2. Entries correspond to @params@ type, i.e. any 'SortingItem' entry of the underlying
list with name "N" will be present in @params@.

Not all parameters specified by @params@ phantom type can be present, e.g. the underlying
list will be empty if user didn't pass "sortBy" query parameter at all.
-}
newtype SortingSpec (params :: [TyNamedParam *]) = SortingSpec
    { unSortingSpec :: [SortingItem]
    } deriving (Show)

-- | For a given return type of an endpoint get corresponding sorting params.
-- This mapping is sensible, since we usually allow to sort only on fields appearing in
-- endpoint's response.
type family SortingParamTypesOf a :: [TyNamedParam *]

-- | This you will most probably want to specify in API.
type SortingParamsOf a = SortingParams (SortingParamTypesOf a)

-- | This you will most probably want to specify in an endpoint implementation.
type SortingSpecOf a = SortingSpec (SortingParamTypesOf a)
