module Servant.Util.Combinators.Sorting.Base
    ( SortingParams
    , SortParamsExpanded
    , SortingSpec (..)
    , ssBase
    , ssAll
    , SortingOrder (..)
    , NullsSortingOrder (..)
    , SortingItemTagged (..)
    , SortingItem (..)
    , TaggedSortingItemsList
    , SortingOrderType (..)
    , ReifySortingItems (..)
    , BaseSortingToParam
    , AllSortingParams
    , SortingParamProvidedOf
    , SortingParamBaseOf
    , SortingParamsOf
    , SortingSpecOf
    ) where

import Universum

import Data.List (nubBy)
import Fmt (Buildable (..))
import GHC.TypeLits (KnownSymbol)
import Servant (QueryParam, (:>))
import Servant.Server (Tagged (..))
import Servant.Util.Common
import qualified Text.Show

{- | Servant API combinator which allows to accept sorting parameters as a query parameter.

Example: with the following combinator

@
SortingParams ["time" ?: Timestamp, "name" ?: Text] '[]
@

the endpoint can parse "sortBy=-time,+name" or "sortBy=desc(time),asc(name)" formats,
which would mean sorting by mentioned fields lexicographically. All sorting
subparameters are optional, as well as entire "sortBy" parameter.

The second type-level list stands for the base sorting order, it will be applied
in the end disregard the user's input.
It is highly recommended to specify the base sorting that unambigously orders the
result(for example - by the primary key of the database), otherwise pagination
may behave unexpectedly for the client when it specifies no sorting.

If you want the base sorting order to be overridable by the user, you can
put the respective fields in both lists. For example, this combinator:

@
SortingParams
  '["time" ?: Timestamp]
   ["id" ?: '(Id, 'Descendant), "time" ?: '(Timestamp, 'Ascendant)]
@

will sort results lexicographically by @(Down id, time)@, but if the client
specifies sorting by time, you will get sorting by @(time, Down id)@ as the
trailing @"time"@ will not affect anything.

It is preferred to put a base sorting at least by @ID@, this way results will be
more deterministic.

Your handler will be provided with 'SortingSpec' argument which can later be passed
in an appropriate function to perform sorting.
-}
data SortingParams
  (provided :: [TyNamedParam *])
  (base :: [TyNamedParam (SortingOrderType *)])

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
newtype SortingItemTagged (provided :: TyNamedParam *) = SortingItemTagged
    { untagSortingItem :: SortingItem
    } deriving (Show)

instance Buildable SortingItem where
    build SortingItem{..} =
        let order = case siOrder of { Ascendant -> "⯅ "; Descendant -> "⯆ " }
        in order <> build siName

deriving instance Buildable (SortingItemTagged param)

-- | Tagged, because we want to retain list of allowed fields for parsing
-- (in @instance FromHttpApiData@).
type TaggedSortingItemsList provided = Tagged (provided :: [TyNamedParam *]) [SortingItem]

-- | Order of sorting for type-level.
--
-- Its constructors accept the type of thing we order by, e.g. @Asc Id@.
data SortingOrderType k
    = Desc k
    | Asc k

-- | What is passed to an endpoint, contains all sorting parameters provided by a user.
{- Following properties hold:
1. Each entry in the underlying list has a unique name ('siName' field).
2. Entries correspond to @params@ type, i.e. any 'SortingItem' entry of the underlying
list with name "N" will be present in @params@.

Not all parameters specified by @params@ phantom type can be present, e.g. the underlying
list will be empty if user didn't pass "sortBy" query parameter at all. However,
entries from the base sorting are always present.
-}
data SortingSpec
  (provided :: [TyNamedParam *])
  (base :: [TyNamedParam (SortingOrderType *)]) =
    ReifySortingItems base =>
    SortingSpec
    { ssProvided :: [SortingItem]
      -- ^ Sorting items provided by the user (lexicographical order).
    }

instance Show (SortingSpec provided base) where
    show s =
        "SortingSpec {ssProvided = " <> show (ssProvided s) <>
                   ", ssBase = " <> show (ssBase s) <> "}"

-- | Base sorting items, that are present disregard the client's input
-- (lexicographical order).
--
-- This is a sort of virtual field, so such naming.
ssBase :: forall base provided. SortingSpec provided base -> [SortingItem]
ssBase SortingSpec{} = reifySortingItems @base

-- | Requires given type-level items to be valid specification of sorting.
class ReifySortingItems (items :: [TyNamedParam (SortingOrderType *)]) where
    reifySortingItems :: [SortingItem]

instance ReifySortingItems '[] where
    reifySortingItems = []

instance ( ReifySortingOrder order, KnownSymbol name
         , ReifySortingItems items
         ) => ReifySortingItems ('TyNamedParam name (order field) ': items) where
    reifySortingItems =
        SortingItem
        { siName = symbolValT @name
        , siOrder = reifySortingOrder @order
        } : reifySortingItems @items

class ReifySortingOrder (order :: * -> SortingOrderType *) where
    reifySortingOrder :: SortingOrder

instance ReifySortingOrder 'Asc where
    reifySortingOrder = Ascendant

instance ReifySortingOrder 'Desc where
    reifySortingOrder = Descendant

-- | All sorting items with duplicates removed (lexicographical order).
ssAll :: SortingSpec provided base -> [SortingItem]
ssAll s = nubBy ((==) `on` siName) (ssProvided s <> ssBase s)

-- | Maps @base@ params to the form common for @provided@ and @base@.
type family BaseSortingToParam (base :: [TyNamedParam (SortingOrderType *)])
  :: [TyNamedParam *] where
    BaseSortingToParam '[] = '[]
    BaseSortingToParam ('TyNamedParam name (order field) ': xs) =
      'TyNamedParam name field ': BaseSortingToParam xs

-- | All sorting params, provided + base.
--
-- This does not yet remove duplicates from @provided@ and @base@ sets,
-- we wait for specific use cases to decide how to handle this better.
type family AllSortingParams
  (provided :: [TyNamedParam *])
  (base :: [TyNamedParam (SortingOrderType *)])
  :: [TyNamedParam *] where
    AllSortingParams provided base = provided ++ BaseSortingToParam base

-- | For a given return type of an endpoint get corresponding sorting params
-- that can be specified by user.
-- This mapping is sensible, since we usually allow to sort only on fields appearing in
-- endpoint's response.
type family SortingParamProvidedOf a :: [TyNamedParam *]

-- | For a given return type of an endpoint get corresponding base sorting params.
type family SortingParamBaseOf a :: [TyNamedParam (SortingOrderType *)]

-- | This you will most probably want to specify in API.
type SortingParamsOf a = SortingParams (SortingParamProvidedOf a) (SortingParamBaseOf a)

-- | This you will most probably want to specify in an endpoint implementation.
type SortingSpecOf a = SortingSpec (SortingParamProvidedOf a) (SortingParamBaseOf a)
