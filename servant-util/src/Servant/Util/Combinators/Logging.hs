{-# LANGUAGE PolyKinds #-}

-- | Allows to enable logging of requests and responses.
module Servant.Util.Combinators.Logging
    ( -- * Automatic requests logging
      LoggingApi
    , LoggingApiRec
    , HasLoggingServer (..)
    , ServantLogConfig (..)
    , ForResponseLog (..)
    , buildListForResponse
    , buildForResponse
    , ApiHasArgClass (..)
    , ApiCanLogArg (..)
    , addParamLogInfo
    , setInPrefix
    , serverWithLogging
    ) where

import Universum

import Control.Exception.Safe (handleAny)
import Control.Monad.Error.Class (catchError, throwError)
import Data.Default (Default (..))
import Data.Kind (Type)
import Data.Reflection (Reifies (..), reify)
import Data.Swagger (Swagger)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt (Buildable (..), Builder, blockListF, pretty, (+|), (|+), (||+))
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Servant.API ((:<|>) (..), (:>), Capture, Description, NoContent, QueryFlag, QueryParam', Raw,
                    ReflectMethod (..), ReqBody, Summary, Verb)
import Servant.Server (Handler (..), HasServer (..), Server, ServerError (..))
import qualified Servant.Server.Internal as SI
import Servant.Swagger.UI.Core (SwaggerUiHtml)
import System.Console.Pretty (Color (..), Style (..), color, style)

import Servant.Util.Common

-- | Enables logging for server which serves given api.
--
-- `config` is a type at which you have to specify 'ServantLogConfig' via
-- reflection. This way was chosen because the least thing we need in
-- config is 'LoggerName', and we want to have '<>' on 'LoggerName's thus
-- 'KnownSymbol' is not enough.
--
-- This logging will report
--
-- * Request parameters, including request bodies
-- * If execution failed with error, it will be displayed
-- * Details like request method and endpoint execution time
--
-- If user makes request which is not defined it won't be logged. However,
-- I don't find it a great problem, it may impede only in development or on
-- getting acknowledged with api.
data LoggingApi config api

-- | Helper to traverse servant api and apply logging.
data LoggingApiRec config api

newtype ServantLogConfig = ServantLogConfig
    { clcLog :: Text -> IO ()
    }

dullColor :: Color -> Text -> Text
dullColor c = style Faint . color c

gray :: Text -> Text
gray = dullColor White

-- | Used to incrementally collect info about passed parameters.
data ApiParamsLogInfo
      -- | Parameters gathered at current stage.
      -- The first field tells whether have we met '(:<|>)',
      -- the second is path prefix itself,
      -- the third field is the remaining part.
    = ApiParamsLogInfo Bool [Text] [Text]
      -- | Parameters collection failed with reason
      --   (e.g. decoding error)
    | ApiNoParamsLogInfo Text

instance Default ApiParamsLogInfo where
    def = ApiParamsLogInfo False [] []

addParamLogInfo :: Text -> ApiParamsLogInfo -> ApiParamsLogInfo
addParamLogInfo _ failed@ApiNoParamsLogInfo{} = failed
addParamLogInfo paramInfo (ApiParamsLogInfo False path []) =
    ApiParamsLogInfo False (paramInfo : path) []
addParamLogInfo paramInfo (ApiParamsLogInfo inPrefix path infos) =
    ApiParamsLogInfo inPrefix path (paramInfo : infos)

setInPrefix :: ApiParamsLogInfo -> ApiParamsLogInfo
setInPrefix failed@ApiNoParamsLogInfo{}     = failed
setInPrefix infos@(ApiParamsLogInfo _ [] _) = infos
setInPrefix (ApiParamsLogInfo _ path info)  = ApiParamsLogInfo True path info

-- | When it comes to logging responses, returned data may be very large.
-- Log space is valuable (already in testnet we got truncated logs),
-- so we have to care about printing only whose data which may be useful.
newtype ForResponseLog a = ForResponseLog { unForResponseLog :: a }

buildListForResponse
    :: Buildable (ForResponseLog x)
    => (forall a. [a] -> [a]) -> ForResponseLog [x] -> Builder
buildListForResponse truncList (ForResponseLog l) =
    let startNf = if null l then "" else "\n"
        lt = truncList l
        diff = length l - length lt
        mMore | diff == 0 = ""
              | otherwise = "\n    and " +| diff |+ " entries more..."
    in  startNf +| blockListF (map ForResponseLog lt) |+ mMore

buildForResponse :: Buildable a => ForResponseLog a -> Builder
buildForResponse = build . unForResponseLog

instance ( HasServer (LoggingApiRec config api) ctx
         , HasServer api ctx
         ) =>
         HasServer (LoggingApi config api) ctx where
    type ServerT (LoggingApi config api) m = ServerT api m

    route = inRouteServer @(LoggingApiRec config api) route
            (def, )

    hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

-- | Version of 'HasServer' which is assumed to perform logging.
-- It's helpful because 'ServerT (LoggingApi ...)' is already defined for us
-- in actual 'HasServer' instance once and forever.
class HasServer api ctx => HasLoggingServer config api ctx where
    routeWithLog
        :: Proxy (LoggingApiRec config api)
        -> SI.Context ctx
        -> SI.Delayed env (Server (LoggingApiRec config api))
        -> SI.Router env

instance HasLoggingServer config api ctx =>
         HasServer (LoggingApiRec config api) ctx where
    type ServerT (LoggingApiRec config api) m =
         (ApiParamsLogInfo, ServerT api m)

    route = routeWithLog

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy :: Proxy api) pc nt <$> s

instance ( HasLoggingServer config api1 ctx
         , HasLoggingServer config api2 ctx
         ) =>
         HasLoggingServer config (api1 :<|> api2) ctx where
    routeWithLog =
        inRouteServer
            @(LoggingApiRec config api1 :<|> LoggingApiRec config api2)
            route $
            \(paramsInfo, f1 :<|> f2) ->
                let paramsInfo' = setInPrefix paramsInfo
                in (paramsInfo', f1) :<|> (paramsInfo', f2)

instance ( KnownSymbol path
         , HasLoggingServer config res ctx
         ) =>
         HasLoggingServer config (path :> res) ctx where
    routeWithLog =
        inRouteServer @(path :> LoggingApiRec config res) route $
        first updateParamsInfo
      where
        updateParamsInfo =
            let path = toText . symbolVal $ Proxy @path
            in  addParamLogInfo path

-- | Describes a way to log a single parameter.
class ApiHasArgClass subApi =>
      ApiCanLogArg subApi where
    type ApiArgToLog subApi :: *
    type ApiArgToLog subApi = ApiArg subApi

    toLogParamInfo
        :: Buildable (ApiArgToLog subApi)
        => Proxy subApi -> ApiArg subApi -> Text
    default toLogParamInfo
        :: Buildable (ApiArg subApi)
        => Proxy subApi -> ApiArg subApi -> Text
    toLogParamInfo _ param = pretty param

instance KnownSymbol s => ApiCanLogArg (Capture s a)

instance ApiCanLogArg (ReqBody ct a)

instance KnownSymbol cs => ApiCanLogArg (QueryParam' mods cs a) where
    type ApiArgToLog (QueryParam' mods cs a) = a
    toLogParamInfo _ mparam = maybe noEntry pretty mparam
      where
        noEntry = gray "-"

instance KnownSymbol cs => ApiCanLogArg (QueryFlag cs) where
    type ApiArgToLog (QueryFlag cs) = Bool

paramRouteWithLog
    :: forall config api subApi res ctx env.
       ( api ~ (subApi :> res)
       , HasServer (subApi :> LoggingApiRec config res) ctx
       , ApiHasArg subApi res
       , ApiHasArg subApi (LoggingApiRec config res)
       , ApiCanLogArg subApi
       , Buildable (ApiArgToLog subApi)
       )
    => Proxy (LoggingApiRec config api)
    -> SI.Context ctx
    -> SI.Delayed env (Server (LoggingApiRec config api))
    -> SI.Router env
paramRouteWithLog =
    inRouteServer @(subApi :> LoggingApiRec config res) route $
        \(paramsInfo, f) a -> (a `updateParamsInfo` paramsInfo, f a)
  where
    updateParamsInfo a =
        let paramVal = toLogParamInfo (Proxy @subApi) a
            paramName = apiArgName $ Proxy @subApi
            paramInfo = paramName |+ ": " +| paramVal |+ ""
        in addParamLogInfo paramInfo . setInPrefix

instance ( HasServer (subApi :> res) ctx
         , HasServer (subApi :> LoggingApiRec config res) ctx
         , ApiHasArg subApi res
         , ApiHasArg subApi (LoggingApiRec config res)
         , ApiCanLogArg subApi
         , Buildable (ApiArgToLog subApi)
         , subApi ~ apiType (a :: Type)
         ) =>
         HasLoggingServer config (apiType a :> res) ctx where
    routeWithLog = paramRouteWithLog

instance ( HasLoggingServer config res ctx
         , KnownSymbol s
         ) =>
         HasLoggingServer config (QueryFlag s :> res) ctx where
    routeWithLog = paramRouteWithLog

instance HasLoggingServer config res ctx =>
         HasLoggingServer config (Summary s :> res) ctx where
    routeWithLog = inRouteServer @(Summary s :> LoggingApiRec config res) route id

instance HasLoggingServer config res ctx =>
         HasLoggingServer config (Description d :> res) ctx where
    routeWithLog = inRouteServer @(Description d :> LoggingApiRec config res) route id


-- | Unique identifier for request-response pair.
newtype RequestId = RequestId Integer

instance Buildable RequestId where
    build (RequestId i) = "#" +| i |+ ""

-- | We want all servant servers to have non-overlapping ids,
-- so using singleton counter here.
requestsCounter :: TVar Integer
requestsCounter = unsafePerformIO $ newTVarIO 0
{-# NOINLINE requestsCounter #-}

nextRequestId :: MonadIO m => m RequestId
nextRequestId = atomically $ do
    modifyTVar' requestsCounter (+1)
    RequestId <$> readTVar requestsCounter

-- | Modify an action so that it performs all the required logging.
applyServantLogging
    :: ( Reifies config ServantLogConfig
       , ReflectMethod (method :: k)
       )
    => Proxy config
    -> Proxy method
    -> ApiParamsLogInfo
    -> (a -> Text)
    -> Handler a
    -> Handler a
applyServantLogging configP methodP paramsInfo showResponse action = do
    timer <- mkTimer
    reqId <- nextRequestId
    catchErrors reqId timer $ do
        reportRequest reqId
        res <- action
        reportResponse reqId timer res
        return res
  where
    method = decodeUtf8 $ reflectMethod methodP :: Text
    cmethod =
        let c = case method of
              "GET"    -> Cyan
              "POST"   -> Yellow
              "PUT"    -> Green
              "DELETE" -> Red
              _        -> Magenta
        in style Faint $ color c method
    mkTimer :: MonadIO m => m (m Text)
    mkTimer = do
        startTime <- liftIO getPOSIXTime
        return $ do
            endTime <- liftIO getPOSIXTime
            return . show $ endTime - startTime
    log :: Text -> Handler ()
    log = liftIO ... clcLog $ reflect configP
    eParamLogs :: Either Text Text
    eParamLogs = case paramsInfo of
        ApiParamsLogInfo _ path infos -> Right $
            let pathPart =
                    "    " <> gray ":>" <> " " <>
                    T.intercalate (gray "/") (reverse path)
                infoPart = reverse infos <&> \info ->
                    "    " +| gray ":>"
                    |+ " " +| info |+ ""
            in T.intercalate "\n" (pathPart : infoPart)
        ApiNoParamsLogInfo why -> Left why
    reportRequest :: RequestId -> Handler ()
    reportRequest reqId =
        case eParamLogs of
            Left e ->
                log $ "\n" +| dullColor Red "Unexecuted request due to error"
                    |+ " " +| e
                    |+ ""
            Right paramLogs -> do
                log $  "\n" +| cmethod
                    |+ " "  +| gray ("Request " <> pretty reqId)
                    |+ "\n" +| paramLogs |+ ""
    responseTag reqId = "Response " <> pretty reqId
    reportResponse reqId timer resp = do
        durationText <- timer
        log $
            "\n    " +| gray (responseTag reqId)
              |+ " " +| (dullColor Green "OK")
              |+ " " +| durationText
              |+ " " +| gray ">"
              |+ " " +| showResponse resp
              |+ ""
    catchErrors reqId st =
        flip catchError (servantErrHandler reqId st) .
        handleAny (exceptionsHandler reqId st)
    servantErrHandler reqId timer err@ServerError{..} = do
        durationText <- timer
        let errMsg = errHTTPCode |+ " "  +| errReasonPhrase |+ ":"
        log $
            "\n    " +| gray (responseTag reqId)
              |+ " " +| durationText
              |+ " " +| dullColor Red errMsg
              |+ " " +| decodeUtf8 @Text errBody
              |+ ""
        throwError err
    exceptionsHandler reqId timer e = do
        durationText <- timer
        log $
            "\n    " +| dullColor Red (responseTag reqId)
              |+ " " +| e
             ||+ " " +| durationText
              |+ ""
        throwM e

applyLoggingToHandler
    :: forall config method a.
       ( Buildable (ForResponseLog a)
       , Reifies config ServantLogConfig
       , ReflectMethod method
       )
    => Proxy config -> Proxy (method :: k) -> (ApiParamsLogInfo, Handler a) -> Handler a
applyLoggingToHandler configP methodP (paramsInfo, handler) = do
    applyServantLogging configP methodP paramsInfo (pretty . ForResponseLog) handler

skipLogging :: (ApiParamsLogInfo, action) -> action
skipLogging = snd

instance ( HasServer (Verb mt st ct a) ctx
         , Reifies config ServantLogConfig
         , ReflectMethod mt
         , Buildable (ForResponseLog a)
         ) =>
         HasLoggingServer config (Verb (mt :: k) (st :: Nat) (ct :: [*]) a) ctx where
    routeWithLog =
        inRouteServer @(Verb mt st ct a) route $
        applyLoggingToHandler (Proxy @config) (Proxy @mt)

instance HasLoggingServer config Raw ctx where
    routeWithLog = inRouteServer @Raw route skipLogging

instance Buildable (ForResponseLog NoContent) where
    build _ = "<no response>"

instance Buildable (ForResponseLog ()) where
    build _ = "<no response>"

instance Buildable (ForResponseLog Integer) where
    build = buildForResponse

instance Buildable (ForResponseLog Swagger) where
    build _ = "Swagger specification"

instance Buildable (ForResponseLog (SwaggerUiHtml dir api)) where
    build _ = "Accessed documentation UI"

-- | Apply logging to the given server.
serverWithLogging
    :: forall api a.
       ServantLogConfig
    -> Proxy api
    -> (forall (config :: Type). Reifies config ServantLogConfig =>
        Proxy (LoggingApi config api) -> a)
    -> a
serverWithLogging config _ f =
    reify config $ \(Proxy :: Proxy config) -> f (Proxy @(LoggingApi config api))
