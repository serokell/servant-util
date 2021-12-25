module Test.Servant.Helpers
    ( runTestServer
    , runTestServerE
    , runTestServer'
    ) where

import Universum

import Control.Monad.Except (liftEither)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Servant.API.Generic (GenericServant, ToServant, ToServantApi)
import Servant.Client (BaseUrl (..), Client, ClientEnv, ClientError, ClientM, HasClient,
                       Scheme (Http), mkClientEnv, runClientM)
import Servant.Client.Generic (AsClientT, genericClientHoist)
import Servant.Server (HasServer, Server, serve)
import Servant.Server.Generic (AsServer, genericServe)

mkTestClientEnv :: Port -> IO ClientEnv
mkTestClientEnv port = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl
        { baseUrlScheme = Http
        , baseUrlHost = "localhost"
        , baseUrlPort = port
        , baseUrlPath = ""
        }
  return $ mkClientEnv manager baseUrl

-- | Runs server and returns client handlers to it.
--
-- This version returns client handlers where errors are returned
-- explicitly.
runTestServerE
  :: ( GenericServant methods AsServer
     , HasServer (ToServantApi methods) '[]
     , Server (ToServantApi methods) ~ ToServant methods AsServer
     , GenericServant methods (AsClientT (ExceptT ClientError IO))
     , HasClient ClientM (ToServantApi methods)
     , Client (ExceptT ClientError IO) (ToServantApi methods)
         ~ ToServant methods (AsClientT (ExceptT ClientError IO))
     )
  => methods AsServer
  -> (methods (AsClientT (ExceptT ClientError IO)) -> IO ())
  -> IO ()
runTestServerE handlers acceptClientHandlers =
  testWithApplication (pure $ genericServe handlers) $ \port -> do
    cliEnv <- mkTestClientEnv port
    acceptClientHandlers $
      genericClientHoist (lift . flip runClientM cliEnv >=> liftEither)

-- | Runs server and returns client handlers to it.
--
-- In case a client handler fails, the exception will be propagated.
runTestServer
  :: ( GenericServant methods AsServer
     , HasServer (ToServantApi methods) '[]
     , Server (ToServantApi methods) ~ ToServant methods AsServer
     , GenericServant methods (AsClientT IO)
     , HasClient ClientM (ToServantApi methods)
     , Client IO (ToServantApi methods) ~ ToServant methods (AsClientT IO)
     )
  => methods AsServer
  -> (methods (AsClientT IO) -> IO ())
  -> IO ()
runTestServer handlers acceptClientHandlers =
  testWithApplication (pure $ genericServe handlers) $ \port -> do
    cliEnv <- mkTestClientEnv port
    acceptClientHandlers $
      genericClientHoist (flip runClientM cliEnv >=> either throwM pure)

-- | Runs server and returns client handlers to it.
--
-- This is similar to 'runTestServer', but accepts any server that matches the client,
-- not necessarily in servant-generic format (which is a quite tough restriction).
runTestServer'
  :: forall api methods.
     ( HasServer api '[]
     , ToServant methods AsServer ~ Server api
     , GenericServant methods (AsClientT IO)
     , HasClient ClientM (ToServantApi methods)
     , Client IO (ToServantApi methods) ~ ToServant methods (AsClientT IO)
     )
  => Proxy api
  -> Server api
  -> (methods (AsClientT IO) -> IO ())
  -> IO ()
runTestServer' apiP handlers acceptClientHandlers =
  testWithApplication (pure $ serve apiP handlers) $ \port -> do
    cliEnv <- mkTestClientEnv port
    acceptClientHandlers $
      genericClientHoist (flip runClientM cliEnv >=> either throwM pure)
