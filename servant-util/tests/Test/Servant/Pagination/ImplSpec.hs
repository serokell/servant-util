{-# LANGUAGE OverloadedLists #-}

module Test.Servant.Pagination.ImplSpec where

import Universum

import qualified Data.Aeson as Aeson
import Servant.API (Get, JSON, (:>))
import Servant.API.Generic (ToServantApi, (:-))
import Servant.Server.Generic (AsServer)
import Servant.Swagger (toSwagger)

import Test.Hspec (Spec, aroundAll, describe, it)
import Test.Hspec.Expectations (shouldBe)

import Servant.Util
import Servant.Util.Dummy

import Test.Servant.Helpers

-- Server
---------------------------------------------------------------------------

data ApiMethods route = ApiMethods
  { amSimplePaginate
      :: route
      :- "simple"
      :> PaginationParams 'DefUnlimitedPageSize
      :> Get '[JSON] [Int]

  , amDefPageSizePaginate
      :: route
      :- "defPageSize"
      :> PaginationParams ('DefPageSize 3)
      :> Get '[JSON] [Int]

  } deriving Generic

apiHandlers :: ApiMethods AsServer
apiHandlers = ApiMethods
  { amSimplePaginate = \paginationSpec -> do
      return $ paginate paginationSpec [1..10]

  , amDefPageSizePaginate = \paginationSpec -> do
      return $ paginate paginationSpec [1..10]
  }

-- Swagger
---------------------------------------------------------------------------

-- You can try this with ghci
printPaginationSwagger :: IO ()
printPaginationSwagger =
  writeFile "pagination-test-swagger.json" $
    decodeUtf8 . Aeson.encode $
    toSwagger $ Proxy @(ToServantApi ApiMethods)

-- Tests
---------------------------------------------------------------------------

spec :: Spec
spec =
  aroundAll (runTestServer apiHandlers) $ do

    describe "Simple pagination" $ do

      it "No specific pagination" $
        \ApiMethods{..} -> do
          res <- amSimplePaginate defPageSize
          res `shouldBe` [1..10]

      it "Set page size" $
        \ApiMethods{..} -> do
          res <- amSimplePaginate (itemsOnPage 2)
          res `shouldBe` [1..2]

      it "Set offset" $
        \ApiMethods{..} -> do
          res <- amSimplePaginate (skipping 5 defPageSize)
          res `shouldBe` [6..10]

      it "Set page size and offset" $
        \ApiMethods{..} -> do
          res <- amSimplePaginate (skipping 5 $ itemsOnPage 3)
          res `shouldBe` [6..8]

    describe "Simple pagination" $ do

      it "No specific pagination" $
        \ApiMethods{..} -> do
          res <- amDefPageSizePaginate defPageSize
          res `shouldBe` [1..3]

      it "Set page size" $
        \ApiMethods{..} -> do
          res <- amDefPageSizePaginate (itemsOnPage 5)
          res `shouldBe` [1..5]

      it "Set offset" $
        \ApiMethods{..} -> do
          res <- amDefPageSizePaginate (skipping 5 defPageSize)
          res `shouldBe` [6..8]

      it "Set page size and offset" $
        \ApiMethods{..} -> do
          res <- amDefPageSizePaginate (skipping 5 $ itemsOnPage 1)
          res `shouldBe` [6]
