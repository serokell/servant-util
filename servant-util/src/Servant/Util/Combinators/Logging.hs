{-# LANGUAGE PolyKinds #-}

-- | Allows to enable logging of requests and responses.
module Servant.Util.Combinators.Logging
    ( -- * Automatic requests logging
      LoggingApi
    , LoggingApiRec
    , LoggingMod
    , LoggingLevel
    , LoggingRequestsEnabled
    , LoggingRequestsDisabled
    , LoggingResponsesEnabled
    , LoggingResponsesDisabled
    , LoggingDisabled
    , LogContext (..)
    , HasLoggingServer (..)
    , ServantLogConfig (..)
    , ForResponseLog (..)
    , BuildableForResponseIfNecessary
    , buildListForResponse
    , buildForResponse
    , ApiHasArgClass (..)
    , ApiCanLogArg (..)
    , addParamLogInfo
    , setInPrefix
    , serverWithLogging
    ) where

import Universum

import Control.Monad.Error.Class (catchError, throwError)
import Data.Constraint (Dict (..))
import Data.Default (Default (..))
import Data.Reflection (Reifies (..), reify)
import Data.Swagger (Swagger)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt (Buildable (..), Builder, blockListF, pretty, (+|), (|+), (||+))
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Servant.API (Capture, Description, NoContent, NoContentVerb, QueryFlag, QueryParam', Raw,
                    ReflectMethod (..), ReqBody, SBoolI, Summary, Verb, (:<|>) (..), (:>))
import Servant.API.Modifiers (FoldRequired, foldRequiredArgument)
import Servant.Client (HasClient (..))
import Servant.Server (Handler (..), HasServer (..), Server, ServerError (..))
import Servant.Swagger (HasSwagger (..))
import Servant.Swagger.UI.Core (SwaggerUiHtml)
import System.Console.Pretty (Color (..), Style (..), color, style)

import qualified Data.Text as T
import qualified Servant.Server.Internal as SI

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
data LoggingApiRec config (lcontext :: LoggingContext) api

-- | Logging context at type-level, accumulates all the 'LoggingMod' modifiers.
data LoggingContext = LoggingContext
  (Maybe Nat)  -- ^ Recommended logging level.
  Bool  -- ^ Whether requests are logged.
  Bool  -- ^ Whether responses are logged.

type family EmptyLoggingContext :: LoggingContext where
  EmptyLoggingContext = 'LoggingContext 'Nothing 'True 'True

type family LcResponsesEnabled (lcontext :: LoggingContext) :: Bool where
  LcResponsesEnabled ('LoggingContext _ _ flag) = flag

-- | Require 'Buildable' for the response type, but only if logging context
-- assumes that the response will indeed be built.
type BuildableForResponseIfNecessary lcontext resp =
  ( If (LcResponsesEnabled lcontext)
      (Buildable (ForResponseLog resp))
      (() :: Constraint)
  , Demote (LcResponsesEnabled lcontext)
  )

-- | Servant combinator that changes how the logs will be printed for the
-- affected endpoints.
--
-- This is an internal thing, we export aliases.
data LoggingMod (mod :: LoggingModKind)

-- | How to change the logging of the endpoints.
data LoggingModKind
  = LMLoggingLevel Nat
  | LMRequestsLogged Bool
  | LMResponsesLogged Bool
  | LMLoggingDisabled

-- | Combinator to set the logging level within the endpoints.
type LoggingLevel lvl = LoggingMod ('LMLoggingLevel lvl)

-- | Combinator to disable logging of requests.
type LoggingRequestsDisabled = LoggingMod ('LMRequestsLogged 'False)

-- | Combinator to enable logging of requests back for a narrower
-- set of entrypoints.
type LoggingRequestsEnabled = LoggingMod ('LMRequestsLogged 'True)

-- | Combinator to disable logging of responses.
type LoggingResponsesDisabled = LoggingMod ('LMResponsesLogged 'False)

-- | Combinator to enable logging of responses.
type LoggingResponsesEnabled = LoggingMod ('LMResponsesLogged 'True)

-- | Combinator to disable all the logging.
--
-- This works similarly to other similar combinators and can be partially
-- or fully reverted with 'LoggingRequestsDisabled' or 'LoggingResponsesDisabled'.
type LoggingDisabled = LoggingMod 'LMLoggingDisabled

-- | Full context of the logging action.
data LogFullContext = LogFullContext
  { lcRecommendedLevel :: Maybe Natural
    -- ^ Logging level specified by 'LoggingLevel' combinator.
    -- Will be provided to the user.
  , lcRequestsEnabled  :: Bool
    -- ^ Whether to log requests.
    -- Accounted automatically.
  , lcResponsesEnabled :: Bool
    -- ^ Whether to log responses.
    -- Accounted automatically.
  } deriving (Show)

type instance Demoted LoggingContext = LogFullContext
instance ( ctx ~ 'LoggingContext lvl req resp
         , Demote lvl
         , Demote req
         , Demote resp
         ) => Demote ctx where
  demote _ = LogFullContext
    { lcRecommendedLevel = demote (Proxy @lvl)
    , lcRequestsEnabled = demote (Proxy @req)
    , lcResponsesEnabled = demote (Proxy @resp)
    }

type family ApplyLoggingMod (lcontext :: LoggingContext) (mod :: LoggingModKind) where
  ApplyLoggingMod ('LoggingContext _ req resp) ('LMLoggingLevel lvl) =
    'LoggingContext ('Just lvl) req resp
  ApplyLoggingMod ('LoggingContext mlvl _ resp) ('LMRequestsLogged req) =
    'LoggingContext mlvl req resp
  ApplyLoggingMod ('LoggingContext mlvl req _) ('LMResponsesLogged resp) =
    'LoggingContext mlvl req resp
  ApplyLoggingMod ('LoggingContext mlvl _ _) 'LMLoggingDisabled =
    'LoggingContext mlvl 'False 'False

-- | Logging context that will be supplied to the user.
newtype LogContext = LogContext
  { lecRecommendedLevel :: Maybe Natural
    -- ^ Logging level specified by 'LoggingLevel' combinator.
  } deriving (Show, Eq)

-- | Logging configuration specified at server start.
newtype ServantLogConfig = ServantLogConfig
    { clcLog :: LogContext -> Text -> IO ()
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

instance ( HasServer (LoggingApiRec config EmptyLoggingContext api) ctx
         , HasServer api ctx
         ) =>
         HasServer (LoggingApi config api) ctx where
    type ServerT (LoggingApi config api) m = ServerT api m

    route = inRouteServer @(LoggingApiRec config EmptyLoggingContext api) route
            (def, )

    hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

-- | Version of 'HasServer' which is assumed to perform logging.
-- It's helpful because 'ServerT (LoggingApi ...)' is already defined for us
-- in actual 'HasServer' instance once and forever.
class HasServer api ctx => HasLoggingServer config (lcontext :: LoggingContext) api ctx where
    routeWithLog
        :: Proxy (LoggingApiRec config lcontext api)
        -> SI.Context ctx
        -> SI.Delayed env (Server (LoggingApiRec config lcontext api))
        -> SI.Router env

instance HasLoggingServer config lcontext api ctx =>
         HasServer (LoggingApiRec config lcontext api) ctx where
    type ServerT (LoggingApiRec config lcontext api) m =
         (ApiParamsLogInfo, ServerT api m)

    route = routeWithLog

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy :: Proxy api) pc nt <$> s

instance ( HasLoggingServer config lcontext api1 ctx
         , HasLoggingServer config lcontext api2 ctx
         ) =>
         HasLoggingServer config lcontext (api1 :<|> api2) ctx where
    routeWithLog =
        inRouteServer
            @(LoggingApiRec config lcontext api1 :<|> LoggingApiRec config lcontext api2)
            route $
            \(paramsInfo, f1 :<|> f2) ->
                let paramsInfo' = setInPrefix paramsInfo
                in (paramsInfo', f1) :<|> (paramsInfo', f2)

instance ( KnownSymbol path
         , HasLoggingServer config lcontext res ctx
         ) =>
         HasLoggingServer config lcontext (path :> res) ctx where
    routeWithLog =
        inRouteServer @(path :> LoggingApiRec config lcontext res) route $
        first updateParamsInfo
      where
        updateParamsInfo =
            let path = toText . symbolVal $ Proxy @path
            in  addParamLogInfo path

-- | Describes a way to log a single parameter.
class ApiHasArgClass subApi =>
      ApiCanLogArg subApi where
    type ApiArgToLog subApi :: Type
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

instance ( Buildable a
         , KnownSymbol cs
         , SBoolI (FoldRequired mods)
         ) =>
         ApiCanLogArg (QueryParam' mods cs a) where
    type ApiArgToLog (QueryParam' mods cs a) = a
    toLogParamInfo _ mparam = foldRequiredArgument (Proxy :: Proxy mods) (\(a :: a) -> pretty a)
      (\case
        Just a  -> pretty a
        Nothing -> noEntry) mparam
      where
        noEntry = gray "-"

instance KnownSymbol cs => ApiCanLogArg (QueryFlag cs) where
    type ApiArgToLog (QueryFlag cs) = Bool

paramRouteWithLog
    :: forall config lcontext api subApi res ctx env.
       ( api ~ (subApi :> res)
       , HasServer (subApi :> LoggingApiRec config lcontext res) ctx
       , ApiHasArg subApi res
       , ApiHasArg subApi (LoggingApiRec config lcontext res)
       , ApiCanLogArg subApi
       , Buildable (ApiArgToLog subApi)
       )
    => Proxy (LoggingApiRec config lcontext api)
    -> SI.Context ctx
    -> SI.Delayed env (Server (LoggingApiRec config lcontext api))
    -> SI.Router env
paramRouteWithLog =
    inRouteServer @(subApi :> LoggingApiRec config lcontext res) route $
        \(paramsInfo, f) a -> (a `updateParamsInfo` paramsInfo, f a)
  where
    updateParamsInfo a =
        let paramVal = toLogParamInfo (Proxy @subApi) a
            paramName = apiArgName $ Proxy @subApi
            paramInfo = paramName |+ ": " +| paramVal |+ ""
        in addParamLogInfo paramInfo . setInPrefix

instance ( HasServer (subApi :> res) ctx
         , HasServer (subApi :> LoggingApiRec config lcontext res) ctx
         , ApiHasArg subApi res
         , ApiHasArg subApi (LoggingApiRec config lcontext res)
         , ApiCanLogArg subApi
         , Buildable (ApiArgToLog subApi)
         , subApi ~ apiType (a :: Type)
         ) =>
         HasLoggingServer config lcontext (apiType a :> res) ctx where
    routeWithLog = paramRouteWithLog

instance ( HasLoggingServer config lcontext res ctx
         , KnownSymbol s
         ) =>
         HasLoggingServer config lcontext (QueryFlag s :> res) ctx where
    routeWithLog = paramRouteWithLog

instance HasLoggingServer config lcontext res ctx =>
         HasLoggingServer config lcontext (Summary s :> res) ctx where
    routeWithLog = inRouteServer @(Summary s :> LoggingApiRec config lcontext res) route id

instance HasLoggingServer config lcontext res ctx =>
         HasLoggingServer config lcontext (Description d :> res) ctx where
    routeWithLog = inRouteServer @(Description d :> LoggingApiRec config lcontext res) route id

instance ( HasLoggingServer config (ApplyLoggingMod lcontext mod) res ctx
         , HasServer res ctx
         ) =>
         HasLoggingServer config lcontext (LoggingMod mod :> res) ctx where
    routeWithLog = inRouteServer @(LoggingApiRec config (ApplyLoggingMod lcontext mod) res) route id

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
       , Demote (lcontext :: LoggingContext)
       , Demote (LcResponsesEnabled lcontext)
       , ReflectMethod (method :: k)
       )
    => Proxy config
    -> Proxy lcontext
    -> Proxy method
    -> ApiParamsLogInfo
    -> If (LcResponsesEnabled lcontext) (a -> Text) ()
    -> Handler a
    -> Handler a
applyServantLogging configP (contextP :: Proxy lcontext) methodP paramsInfo showResponse action = do
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
    logContext = demote contextP
    logEntryContext = LogContext
      { lecRecommendedLevel = lcRecommendedLevel logContext
      }
    log :: Text -> Handler ()
    log txt = liftIO $ clcLog (reflect configP) logEntryContext txt
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
      when (lcRequestsEnabled logContext) $
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
    reportResponse reqId timer resp =
      case tyBoolCase (Proxy @(LcResponsesEnabled lcontext)) of
        Left _ -> pass
        Right Dict -> do
          durationText <- timer
          log $
              "\n    " +| gray (responseTag reqId)
                |+ " " +| dullColor Green "OK"
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
    :: forall k config lcontext method a.
       ( Reifies config ServantLogConfig
       , Demote lcontext
       , BuildableForResponseIfNecessary lcontext a
       , Demote lcontext
       , ReflectMethod method
       )
    => Proxy config -> Proxy (lcontext :: LoggingContext) -> Proxy (method :: k)
    -> (ApiParamsLogInfo, Handler a) -> Handler a
applyLoggingToHandler configP contextP methodP (paramsInfo, handler) = do
    let apply format =
          applyServantLogging configP contextP methodP paramsInfo format handler
    case tyBoolCase $ Proxy @(LcResponsesEnabled lcontext) of
        Right Dict -> apply (pretty . ForResponseLog)
        Left Dict  -> apply ()

skipLogging :: (ApiParamsLogInfo, action) -> action
skipLogging = snd

instance ( HasServer (Verb mt st ct a) ctx
         , Reifies config ServantLogConfig
         , Demote lcontext
         , ReflectMethod mt
         , BuildableForResponseIfNecessary lcontext a
         ) =>
         HasLoggingServer config lcontext (Verb (mt :: k) (st :: Nat) (ct :: [Type]) a) ctx where
    routeWithLog =
        inRouteServer @(Verb mt st ct a) route $
        applyLoggingToHandler (Proxy @config) (Proxy @lcontext) (Proxy @mt)

instance ( HasServer (NoContentVerb mt) ctx
         , Reifies config ServantLogConfig
         , Demote lcontext
         , ReflectMethod mt
         , BuildableForResponseIfNecessary lcontext NoContent
         ) =>
         HasLoggingServer config lcontext (NoContentVerb (mt :: k)) ctx where
    routeWithLog =
        inRouteServer @(NoContentVerb mt) route $
        applyLoggingToHandler (Proxy @config) (Proxy @lcontext) (Proxy @mt)

instance HasLoggingServer config lcontext Raw ctx where
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

instance HasServer api ctx =>
         HasServer (LoggingMod mod :> api) ctx where
    type ServerT (LoggingMod mod :> api) m = ServerT api m
    route = inRouteServer @api route id
    hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance HasClient m api =>
         HasClient m (LoggingMod mod :> api) where
    type Client m (LoggingMod mod :> api) = Client m api
    clientWithRoute mp _ = clientWithRoute mp (Proxy @api)
    hoistClientMonad mp _ = hoistClientMonad mp (Proxy @api)

instance HasSwagger api =>
         HasSwagger (LoggingMod mod :> api) where
    toSwagger _ = toSwagger (Proxy @api)

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
