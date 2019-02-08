{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeInType #-}

-- | Allows complex filtering on specified fields.
module Servant.Util.Combinators.Filtering
    ( FilterKind (..)
    , TyNamedFilter
    , FilteringParams
    , SupportedFilters
    , IsAutoFilter (..)
    , SomeTypeFilter (..)
    , SomeFilter (..)
    , FilteringSpec (..)
    , mapSomeTypeFilter

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

import Universum

import qualified Data.Text as T
import Servant.Client (HasClient (..))
import Data.Kind (type (*))
import Servant (HasServer (..), (:>), FromHttpApiData(..), err400, ServantErr(..))
import Network.Wai.Internal (rawQueryString)
import Servant.Server.Internal (addParameterCheck, withRequest, delayedFailFatal)
import Fmt ((+|),(|+))
import Data.Time.Clock (UTCTime)
import Network.HTTP.Types.URI (QueryText, parseQueryText)
import qualified Data.Map as M
import GHC.TypeLits (KnownSymbol)

import Servant.Util.Common

-- TODO: map values we filter on

-- | We support two kinds of filters.
data FilterKind a
    = AutoFilter a
      -- ^ Automatic filter where different operations are supported (eq, in, cmp).
      -- When applied to backend, only filtered value should be supplied.
    | ManualFilter a
      -- ^ User-provided value is passed to backend implementation as-is,
      -- and filtering on this value should be written manually.

type TyNamedFilter = TyNamedParam (FilterKind *)

-- | Servant API combinator which enables filtering on given fields.
--
-- If type @T@ appears with a name @name@ in @params@ argument, then query parameters of
-- @name[op]=value@ format will be accepted, where @op@ is a filtering operation
-- (e.g. equal, not equal, greater) and @value@ is an item of type @T@ we filter against.
-- Multiple filters will form a conjunction.
--
-- List of allowed filtering operations depends on type @T@ and is specified by
-- 'SupportedFilters' type family.
--
-- Operation argument is optional, when not specified "equality" filter is applied.
--
-- Endpoint implementation will receive 'FilteringSpec' value which contains information
-- about all filters passed by user. You can later put it to an appropriate function
-- to apply filtering.
data FilteringParams (params :: [TyNamedFilter])

-- | For a type of field, get a list of supported filtering operations on this field.
type family SupportedFilters ty :: [* -> *]

-- | Support for @(==)@, @(/=)@ and @IN <values list>@ operations.
data FilterMatching a
    = FilterMatching a
    | FilterNotMatching a
    | FilterItemsIn [a]
    deriving (Functor)

-- | Support for @(<)@, @(>)@, @(<=)@ and @(>=)@ operations.
data FilterComparing a
    = FilterGT a
    | FilterLT a
    | FilterGTE a
    | FilterLTE a
    deriving (Functor)

newtype LikeFormatter = LikeFormatter { unLikeFormatter :: Text }

-- | Support for SQL's LIKE syntax.
data FilterOnLikeTemplate a
    = FilterOnLikeTemplate LikeFormatter
    deriving (Functor)

type NumericFilterTypes = [FilterMatching, FilterComparing]
type TextFilterTypes = [FilterMatching, FilterComparing]
type DatetimeFilterTypes = '[FilterComparing]

type instance SupportedFilters Bool = '[FilterMatching]
type instance SupportedFilters Int = NumericFilterTypes
type instance SupportedFilters Text = TextFilterTypes
type instance SupportedFilters ByteString = TextFilterTypes
type instance SupportedFilters UTCTime = DatetimeFilterTypes

-- | Parses text on the right side of "=" sign in query parameters.
newtype FilteringValueParser a = FilteringValueParser (Text -> Either Text a)
    deriving (Functor)

-- | Delegate to 'FromHttpApiData'.
parseFilteringValueAsIs :: FromHttpApiData a => FilteringValueParser a
parseFilteringValueAsIs = FilteringValueParser parseUrlPiece

-- | Application of a filter type to Servant API.
class Typeable filter => IsAutoFilter (filter :: * -> *) where
    -- | For each supported filtering operation specifies parser for a filtering value.
    autoFilterParsers
        :: FromHttpApiData a
        => Proxy filter -> Map Text $ FilteringValueParser (filter a)

    mapAutoFilterValue
        :: (a -> b) -> filter a -> filter b

-- | If no filtering command specified, think like if the given one was passed.
defFilteringCmd :: Text
defFilteringCmd = "eq"

instance IsAutoFilter FilterMatching where
    autoFilterParsers _ = M.fromList
        [ ( defFilteringCmd
          , FilterMatching <$> parseFilteringValueAsIs
          )
        , ( "neq"
          , FilterNotMatching <$> parseFilteringValueAsIs
          )
        , ( "in"
          , FilterItemsIn <$> FilteringValueParser parseValuesList
          )
        ]
      where
        parseValuesList text = do
            text' <- maybeToRight ("Expected comma-separated list within '[]'") $
                T.stripPrefix "[" text >>= T.stripSuffix "]"
            let vals = T.splitOn "," text'
            mapM parseUrlPiece vals

    mapAutoFilterValue = fmap

instance IsAutoFilter FilterComparing where
    autoFilterParsers _ = M.fromList
        [ ( "gt"
          , FilterGT <$> parseFilteringValueAsIs
          )
        , ( "lt"
          , FilterLT <$> parseFilteringValueAsIs
          )
        , ( "gte"
          , FilterGTE <$> parseFilteringValueAsIs
          )
        , ( "lte"
          , FilterLTE <$> parseFilteringValueAsIs
          )
        ]

    mapAutoFilterValue = fmap

-- instance IsAutoFilter FilterOnLikeTemplate where
--     autoFilterParsers _ = M.fromList
--         [ ( "like"
--           , undefined

--           )
--         ]

-- | Multi-version of 'IsFilter'.
class AreAutoFilters (filters :: [* -> *]) where
    mapFilterTypes
        :: (forall filter. IsAutoFilter filter => Proxy filter -> a)
        -> Proxy filters -> [a]

instance AreAutoFilters '[] where
    mapFilterTypes _ _ = []

instance (IsAutoFilter filter, AreAutoFilters filters) =>
         AreAutoFilters (filter ': filters) where
    mapFilterTypes mapper _ =
        mapper (Proxy @filter) : mapFilterTypes mapper (Proxy @filters)

-- | Some filter for an item of type @a@.
-- Filter type is guaranteed to be one of @SupportedFilters a@.
data SomeTypeFilter a
    = forall filter. IsAutoFilter filter => SomeTypeAutoFilter (filter a)
      -- ^ One of automatic filters for type @a@.
    | SomeTypeManualFilter a
      -- ^ Manually implemented filter.

mapSomeTypeFilter :: (a -> b) -> SomeTypeFilter a -> SomeTypeFilter b
mapSomeTypeFilter f (SomeTypeAutoFilter filtr) = SomeTypeAutoFilter (mapAutoFilterValue f filtr)
mapSomeTypeFilter f (SomeTypeManualFilter v) = SomeTypeManualFilter (f v)

autoFiltersParsers
    :: forall filters a.
       (AreAutoFilters filters, FromHttpApiData a)
    => Map Text $ FilteringValueParser (SomeTypeFilter a)
autoFiltersParsers =
    let parsers = mapFilterTypes (fmap (fmap SomeTypeAutoFilter) . autoFilterParsers)
                                 (Proxy @filters)
    in foldl' (M.unionWithKey onDuplicateCmd) mempty parsers
  where
    onDuplicateCmd cmd = error $ "Different filters have the same command " <> show cmd

-- | Try to parse given query parameter as filter applicable to type @a@.
-- If the parameter is not recognized as filtering one, 'Nothing' is returned.
-- Otherwise it is parsed and any potential errors are reported as-is.
parseAutoTypeFilteringParam
    :: forall a (filters :: [* -> *]).
       (filters ~ SupportedFilters a, AreAutoFilters filters, FromHttpApiData a)
    => Text -> Text -> Text -> Maybe (Either Text $ SomeTypeFilter a)
parseAutoTypeFilteringParam field key val =
    let (field', remainder) = T.break (== '[') key
    in guard (field == field') $> do
        mop <- if null remainder
                then pure Nothing
                else fmap Just $
                     maybeToRight ("Unclosed bracket in query key '" +| key |+ "'") $
                     T.stripSuffix "]" remainder

        let op = mop ?: defFilteringCmd
        let parsersPerOp = autoFiltersParsers @filters @a
        let allowedOps = M.keys parsersPerOp

        FilteringValueParser parser <- case M.lookup op parsersPerOp of
            Nothing -> Left $ "Unsupported filtering command " <> show op <> ". \
                              \Available commands: " <>
                              (T.intercalate ", " $ map show allowedOps)
            Just parser -> pure parser

        parser val
{-# INLINE parseAutoTypeFilteringParam #-}

-- | Some filter.
-- This filter is guaranteed to match a type which is mentioned in @params@.
data SomeFilter (params :: [TyNamedFilter]) where
    SomeFilter :: Typeable a =>
        { sfName :: Text
        , sfFilter :: SomeTypeFilter a
        } -> SomeFilter params

extendSomeFilter :: SomeFilter params -> SomeFilter (param ': params)
extendSomeFilter (SomeFilter f n) = SomeFilter f n

-- | Application of filter params.
class AreFilteringParams (params :: [TyNamedFilter])  where
    -- | Try to parser given query parameter as a filter corresponding to @params@
    -- configuration.
    -- If the query parameter is not recognized as filtering one, 'Nothing' is returned.
    -- Otherwise it is parsed and any potential errors are reported as-is.
    parseFilteringParam :: Text -> Text -> Maybe (Either Text $ SomeFilter params)

instance AreFilteringParams '[] where
    parseFilteringParam _ _ = Nothing
    {-# INLINE parseFilteringParam #-}

instance ( FromHttpApiData ty
         , Typeable ty
         , AreAutoFilters (SupportedFilters ty)
         , KnownSymbol name
         , AreFilteringParams params
         ) =>
         AreFilteringParams ('TyNamedParam name ('AutoFilter ty) ': params) where
    parseFilteringParam key val = asum
        [ fmap (fmap (SomeFilter name)) $
            parseAutoTypeFilteringParam @ty (symbolValT @name) key val

        , fmap (fmap extendSomeFilter) $
            parseFilteringParam @params key val
        ]
      where
        name = symbolValT @name
    {-# INLINE parseFilteringParam #-}

instance ( FromHttpApiData ty
         , Typeable ty
         , KnownSymbol name
         , AreFilteringParams params
         ) =>
         AreFilteringParams ('TyNamedParam name ('ManualFilter ty) ': params) where
    parseFilteringParam key val = asum
        [ fmap (fmap (SomeFilter name)) $
            guard (symbolValT @name == key) $> do
                SomeTypeManualFilter <$> parseUrlPiece @ty val

        , fmap (fmap extendSomeFilter) $
            parseFilteringParam @params key val
        ]
      where
        name = symbolValT @name
    {-# INLINE parseFilteringParam #-}

extractQueryParamsFilters
    :: forall (params :: [TyNamedFilter]).
       (AreFilteringParams params)
    => QueryText -> Either Text [SomeFilter params]
extractQueryParamsFilters qt = sequence $ do
    (key, mvalue) <- qt
    Just value <- pure mvalue
    Just aFilter <- pure $ parseFilteringParam @params key value
    return aFilter
{-# INLINE extractQueryParamsFilters #-}

-- | This is what you get in endpoint implementation, it contains all filters
-- supplied by a user.
-- Invariant: each filter correspond to some type mentioned in @params@.
data FilteringSpec (params :: [TyNamedFilter]) =
    FilteringSpec [SomeFilter params]

instance ( HasServer subApi ctx
         , AreFilteringParams params
         ) =>
         HasServer (FilteringParams params :> subApi) ctx where

    type ServerT (FilteringParams params :> subApi) m =
        FilteringSpec params -> ServerT subApi m

    route _ ctx delayed =
        route (Proxy @subApi) ctx $
        addParameterCheck delayed (withRequest extractParams)
      where
        extractParams req =
            let -- Copy-pasted from 'instance HasServer QueryParam'
                queryText = parseQueryText (rawQueryString req)
            in fmap FilteringSpec . eitherToDelayed $
                   extractQueryParamsFilters @params queryText
        eitherToDelayed = \case
            Left err -> delayedFailFatal err400{ errBody = encodeUtf8 err }
            Right x  -> pure x

    hoistServerWithContext _ pm hst s = hoistServerWithContext (Proxy @subApi) pm hst . s

-- | We do not yet support passing filtering parameters in client.
instance HasClient m subApi =>
         HasClient m (FilteringParams params :> subApi) where
    type Client m (FilteringParams params :> subApi) = Client m subApi
    clientWithRoute mp _ req = clientWithRoute mp (Proxy @subApi) req

-- | For a given return type of an endpoint get corresponding filtering params.
-- This mapping is sensible, since we usually allow to filter only on fields appearing in
-- endpoint's response.
type family FilteringParamTypesOf a :: [TyNamedFilter]

-- | This you will most probably want to specify in API.
type FilteringParamsOf a = FilteringParams (FilteringParamTypesOf a)

-- | This you will most probably want to specify in an endpoint implementation.
type FilteringSpecOf a = FilteringSpec (FilteringParamTypesOf a)
