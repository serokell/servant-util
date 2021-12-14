{-# LANGUAGE OverloadedLists #-}

module Test.Servant.Logging.ImplSpec where

import Universum

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Servant.API (Get, JSON, (:>))
import Servant.API.Generic (ToServantApi, (:-))
import Servant.Client.Generic (AsClientT)
import Servant.Server.Generic (AsServer, genericServer)

import Test.Hspec (Spec, around, it)
import Test.Hspec.Expectations (expectationFailure, shouldBe, shouldSatisfy)

import Servant.Util

import Test.Servant.Helpers

-- Server
---------------------------------------------------------------------------

-- | A type that has no 'Buildable ForResponseLog` instance.
newtype NeverLogMe = NeverLogMe ()
  deriving (ToJSON, FromJSON)

data ApiMethods route = ApiMethods
  { amSimple
      :: route
      :- "simple"
      :> Get '[JSON] ()

  , amSetLevel
      :: route
      :- "setLevel"
      :> LoggingLevel 10
      :> Get '[JSON] ()

  , amRequestsDisabled
      :: route
      :- "-requests"
      :> LoggingRequestsDisabled
      :> Get '[JSON] ()

  , amResponsesDisabled
      :: route
      :- "-responses"
      :> LoggingResponsesDisabled
      :> Get '[JSON] NeverLogMe

  , amFullyDisabled
      :: route
      :- "-allLogs"
      :> LoggingDisabled
      :> Get '[JSON] ()

  } deriving Generic

apiHandlers :: ApiMethods AsServer
apiHandlers = ApiMethods pass pass pass (pure $ NeverLogMe ()) pass

runOurHandlers
  :: ((ApiMethods (AsClientT IO), IO [(LogContext, Text)]) -> IO ())
  -> IO ()
runOurHandlers clientCb = do
  logRecords <- newIORef []
  let config = ServantLogConfig
        { clcLog = \ctx txt -> modifyIORef logRecords ((ctx, txt) : )
        }
  serverWithLogging config (Proxy @(ToServantApi ApiMethods)) $ \apiP ->
    runTestServer' apiP (genericServer apiHandlers) $ \clientMethods ->
      clientCb (clientMethods, reverse <$> readIORef logRecords)

-- Tests
---------------------------------------------------------------------------

spec :: Spec
spec =
  around runOurHandlers $ do

    it "Simple logging" $
      \(ApiMethods{..}, getLogRecords) -> do
        amSimple
        getLogRecords >>= \case
          [(reqCtx, reqTxt), (respCtx, respTxt)] -> do
            reqCtx `shouldBe` LogContext Nothing
            reqTxt `shouldSatisfy` ("simple" `T.isInfixOf`)
            reqTxt `shouldSatisfy` ("Request" `T.isInfixOf`)
            respCtx `shouldBe` LogContext Nothing
            respTxt `shouldSatisfy` ("Response" `T.isInfixOf`)

          other -> expectationFailure $
            "Unexpected number of log entries: " <> show (length other)

    it "Recommended log levels" $
      \(ApiMethods{..}, getLogRecords) -> do
        amSetLevel
        getLogRecords >>= \case
          [(reqCtx, _), (respCtx, _)] -> do
            reqCtx `shouldBe` LogContext (Just 10)
            respCtx `shouldBe` LogContext (Just 10)

          other -> expectationFailure $
            "Unexpected number of log entries: " <> show (length other)

    it "Requests disabled" $
      \(ApiMethods{..}, getLogRecords) -> do
        amRequestsDisabled
        getLogRecords >>= \case
          [(_, _)] ->
            pass
          other -> expectationFailure $
            "Unexpected number of log entries: " <> show (length other)

    it "Responses disabled" $
      \(ApiMethods{..}, getLogRecords) -> do
        amResponsesDisabled
        getLogRecords >>= \case
          [(_, _)] ->
            pass
          other -> expectationFailure $
            "Unexpected number of log entries: " <> show (length other)

    it "Logging fully disabled" $
      \(ApiMethods{..}, getLogRecords) -> do
        amFullyDisabled
        getLogRecords >>= \case
          [] ->
            pass
          other -> expectationFailure $
            "Unexpected number of log entries: " <> show (length other)
