-- | Provides combinator for lexicographical sorting.
module Servant.Util.Combinators.Sorting
    ( SortingParams
    , TyNamedParam (..)
    , type (?:)
    , SortingSpec (..)
    , SortingOrder (..)
    , NullsSortingOrder (..)
    , SortingItemTagged (..)
    , SortingItem (..)
    , SortingParamTypesOf
    , SortingParamsOf
    , SortingSpecOf
    ) where

import Universum

import Data.Char (isAlphaNum)
import Data.Default (Default (..))
import qualified Data.List as L
import qualified Data.Set as S
import Fmt (Buildable (..), fmt)
import Servant.API ((:>), FromHttpApiData (..), QueryParam)
import Servant.Client.Core (Client, HasClient (..))
import Servant.Server (HasServer (..), Tagged (..), unTagged)
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import Servant.Util.Combinators.Logging
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

-- | How servant sees 'SortParams' under the hood.
type SortParamsExpanded allowed subApi =
    QueryParam "sortBy" (TaggedSortingItemsList allowed) :> subApi

{- | What is passed to an endpoint, contains all sorting parameters provided by a user.

Following properties hold:
1. Each entry in the underlying list has a unique name ('siName' field).
2. Entries correspond to @params@ type, i.e. any 'SortingItem' entry of the underlying
list with name "N" will be present in @params@.

Not all parameters specified by @params@ phantom type can be present, e.g. the underlying
list will be empty if user didn't pass "sortBy" query parameter at all.
-}
newtype SortingSpec (params :: [TyNamedParam *]) = SortingSpec
    { unSortingSpec :: [SortingItem]
    } deriving (Default)

-- | Ensure no name in entires repeat.
sortingCheckDuplicates :: [SortingItem] -> Either Text ()
sortingCheckDuplicates items =
    let names = map siName items
        duplicate = safeHead . mapMaybe (safeHead . drop 1) . L.group $ sort names
    in maybe pass (\n -> Left $ "Duplicated field " <> show n) duplicate

-- | Consumes "sortBy" query parameter and fetches sorting parameters contained in it.
instance ( HasServer subApi ctx
         , ReifyParamsNames params
         ) =>
         HasServer (SortingParams params :> subApi) ctx where
    type ServerT (SortingParams params :> subApi) m =
        SortingSpec params -> ServerT subApi m

    route =
        inRouteServer @(SortParamsExpanded params subApi) route $
        \handler rawSortItems -> handler (SortingSpec $ fmap unTagged rawSortItems ?: [])

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy @subApi) pc nt . s

-- | Parse 'sort_by' query param.
-- Following the format described in "Sorting" section of https://www.moesif.com/blog/technical/api-design/REST-API-Design-Filtering-Sorting-and-Pagination/
instance ReifyParamsNames allowed =>
         FromHttpApiData (TaggedSortingItemsList allowed) where
    parseUrlPiece =
        first (toText . P.parseErrorPretty) . second Tagged .
        P.parse parser "sortBy"
      where
        parser = do
            items <- P.sepBy itemParser (P.char ',')
            either (fail . toString) pure $ sortingCheckDuplicates items
            P.eof
            return items

        itemParser :: P.Parsec Void Text SortingItem
        itemParser = asum
            [ do
                siOrder <- asum
                    [ Ascendant <$ P.char '+'
                    , Descendant <$ P.char '-'
                    ] <?> "ordering sign (+/-)"
                siName <- paramNameParser
                return SortingItem{..}

            , do
                siOrder <- asum
                    [ Ascendant <$ P.string' "asc"
                    , Descendant <$ P.string' "desc"
                    ] <?> "ordering keyword (asc/desc)"
                siName <- P.char '(' *> paramNameParser <* P.char ')'
                return SortingItem{..}
            ]

        allowedParams = reifyParamsNames @allowed

        paramNameParser = do
            name <- P.takeWhile1P (Just "sorting item name") isAlphaNum <?> "parameter name"
            unless (name `S.member` allowedParams) $
                fail $ "unknown parameter " <> show name <>
                       " (expected one of " <> show (toList allowedParams) <> ")"
            return name

instance ( HasLoggingServer config subApi ctx
         , ReifyParamsNames params
         ) =>
         HasLoggingServer config (SortingParams params :> subApi) ctx where
    routeWithLog =
        inRouteServer @(SortingParams params :> LoggingApiRec config subApi) route $
        \(paramsInfo, handler) sorting@(SortingSpec params) ->
            let paramLog
                  | null params = "no sorting"
                  | otherwise = fmt . mconcat $
                                "sorting: " : L.intersperse " " (map build params)
            in (addParamLogInfo paramLog paramsInfo, handler sorting)

-- | We do not yet support passing sorting parameters in client.
instance HasClient m subApi =>
         HasClient m (SortingParams params :> subApi) where
    type Client m (SortingParams params :> subApi) = Client m subApi
    clientWithRoute mp _ req = clientWithRoute mp (Proxy @subApi) req

-- | For a given return type of an endpoint get corresponding sorting params.
-- This mapping is sensible, since we usually allow to sort only on fields appearing in
-- endpoint's response.
type family SortingParamTypesOf a :: [TyNamedParam *]

-- | This you will most probably want to specify in API.
type SortingParamsOf a = SortingParams (SortingParamTypesOf a)

-- | This you will most probably want to specify in an endpoint implementation.
type SortingSpecOf a = SortingSpec (SortingParamTypesOf a)
