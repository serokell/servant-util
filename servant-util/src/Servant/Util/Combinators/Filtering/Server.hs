{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Util.Combinators.Filtering.Server
    ( AreFilteringParams
    ) where

import Universum

import Data.Kind (Type)
import qualified Data.Map as M
import qualified Data.Text as T
import Fmt (Buildable (..))
import GHC.TypeLits (KnownSymbol)
import Network.HTTP.Types.URI (QueryText, parseQueryText)
import Network.Wai.Internal (rawQueryString)
import Servant ((:>), FromHttpApiData (..), HasServer (..), ServerError (..), err400)
import Servant.Server.Internal (addParameterCheck, delayedFailFatal, withRequest)

import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Common

-- | Try to parse query key assuming that the specified field to filter on
-- should match the given name.
-- If specified filter does not match the given one, 'Nothing' is returned.
-- Otherwise operation name is extracted and returned.
parseQueryKey :: Text -> Text -> Maybe Text
parseQueryKey field key = do
    remainder <- T.stripPrefix field key
    asum
        [ guard (null remainder) $> DefFilteringCmd
        , T.stripPrefix "[" <=< T.stripSuffix "]" $ remainder
        , T.stripPrefix "_" remainder
        ]

-- | Try to parse given query parameter as filter applicable to type @a@.
-- If the parameter is not recognized as filtering one, 'Nothing' is returned.
-- Otherwise it is parsed and any potential errors are reported as-is.
parseAutoTypeFilteringParam
    :: forall a (filters :: [Type -> Type]).
       (filters ~ SupportedFilters a, AreAutoFilters filters, FromHttpApiData a)
    => Text -> Text -> Text -> Maybe (Either Text $ SomeTypeAutoFilter a)
parseAutoTypeFilteringParam field key val = do
    op <- parseQueryKey field key
    let parsersPerOp = autoFiltersParsers @filters @a
    let allowedOps = M.keys parsersPerOp

    pure $ do
        FilteringValueParser parser <- case M.lookup op parsersPerOp of
            Nothing -> Left $ "Unsupported filtering command " <> show op <> ". \
                              \Available commands: " <>
                              (T.intercalate ", " $ map show allowedOps)
            Just parser -> pure parser

        parser val
{-# INLINE parseAutoTypeFilteringParam #-}

-- | Application of filter params.
class AreFilteringParams (params :: [TyNamedFilter]) where
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
        [ fmap (fmap (SomeFilter name . TypeAutoFilter)) $
            parseAutoTypeFilteringParam @ty (symbolValT @name) key val

        , fmap (fmap extendSomeFilter) $
            parseFilteringParam @params key val
        ]
      where
        name = symbolValT @name
    {-# INLINE parseFilteringParam #-}

instance ( FromHttpApiData ty
         , Buildable ty
         , Typeable ty
         , KnownSymbol name
         , AreFilteringParams params
         ) =>
         AreFilteringParams ('TyNamedParam name ('ManualFilter ty) ': params) where
    parseFilteringParam key val = asum
        [ guard (name == key) $> do
            v <- parseUrlPiece @ty val
            return $ SomeFilter
                { sfName = name
                , sfFilter = TypeManualFilter v
                }

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
