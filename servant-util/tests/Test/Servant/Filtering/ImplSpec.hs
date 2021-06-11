{-# LANGUAGE OverloadedLists #-}

module Test.Servant.Filtering.ImplSpec where

import Universum

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import Data.Swagger (ToSchema)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock (UTCTime (..))
import Servant.API (Get, JSON, (:>))
import Servant.API.Generic (ToServantApi, (:-))
import Servant.Server.Generic (AsServer)
import Servant.Swagger (toSwagger)

import Test.Hspec (Spec, aroundAll, describe, it)
import Test.Hspec.Expectations (shouldBe)

import Servant.Util
import Servant.Util.Dummy

import Test.Servant.Helpers

-- Data model
---------------------------------------------------------------------------

newtype Isbn = Isbn Word64
  deriving (Show, Eq, Ord, Generic)

instance ToSchema Isbn

Aeson.deriveJSON Aeson.defaultOptions ''Isbn

data Book = Book
  { isbn      :: Isbn
  , name      :: Text
  , rating    :: Word8
  , createdAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToSchema Book

Aeson.deriveJSON Aeson.defaultOptions ''Book

-- | Some strictly monotonic function from integers to time.
toUTCTime :: Integer -> UTCTime
toUTCTime idx = UTCTime (fromOrdinalDate idx 0) 0

allBooks :: [Book]
allBooks =
  [ Book
    { isbn = Isbn 1
    , name = "Delving into memes"
    , rating = 5
    , createdAt = toUTCTime 3
    }

  , Book
    { isbn = Isbn 2
    , name = "How to cook tofu, the bright way"
    , rating = 5
    , createdAt = toUTCTime 1
    }

  , Book
    { isbn = Isbn 3
    , name = "Book of something"
    , rating = 4
    , createdAt = toUTCTime 8
    }

  ]

-- Server
---------------------------------------------------------------------------

newtype ApiMethods route = ApiMethods
  { amSimpleFilter
      :: route
      :- "simple"
      :> FilteringParams
          [ "name" ?: 'AutoFilter Text
          , "rating" ?: 'AutoFilter Word8
          , "createdAt" ?: 'AutoFilter UTCTime
          , "isLegendary" ?: 'ManualFilter Bool
          ]
      :> Get '[JSON] [Book]

  } deriving Generic

apiHandlers :: ApiMethods AsServer
apiHandlers = ApiMethods
  { amSimpleFilter = \filterSpec -> do
      let filterApp Book{..} =
            filterOn @"name" name .*.
            filterOn @"rating" rating .*.
            filterOn @"createdAt" createdAt .*.
            manualFilter @"isLegendary"
              (\isLegendary -> (rating == 5) == isLegendary) .*.
            HNil
      return $ filterBySpec filterSpec filterApp allBooks

  }

-- Swagger
---------------------------------------------------------------------------

-- Normally such generic types are not used in API,
-- but in these tests we do not define newtype wrappers.
type instance ParamDescription Text = "Some text"
type instance ParamDescription Word8 = "Rating"
type instance ParamDescription UTCTime = "Creation time"
type instance ParamDescription Bool = "Some flag"

-- You can try this with ghci
printFilterSwagger :: IO ()
printFilterSwagger =
  writeFile "filter-test-swagger.json" $
    decodeUtf8 . Aeson.encode $
    toSwagger $ Proxy @(ToServantApi ApiMethods)

-- Tests
---------------------------------------------------------------------------

spec :: Spec
spec =
  aroundAll (runTestServer apiHandlers) $ do

    describe "Simple filter" $ do

      it "No filtering" $
        \ApiMethods{..} -> do
          res <- amSimpleFilter noFilters
          map isbn res `shouldBe` [Isbn 1, Isbn 2, Isbn 3]

      it "Filtering with exact match" $
        \ApiMethods{..} -> do
          res <- amSimpleFilter [#rating ?/= 4]
          map isbn res `shouldBe` [Isbn 3]

      it "Filtering with comparison" $
        \ApiMethods{..} -> do
          res <- amSimpleFilter [#createdAt ?/>= toUTCTime 5]
          map isbn res `shouldBe` [Isbn 3]

      it "Multiple filters" $
        \ApiMethods{..} -> do
          res <- amSimpleFilter [#rating ?/= 5, #createdAt ?/< toUTCTime 2]
          map isbn res `shouldBe` [Isbn 2]

      it "Manual filter" $
        \ApiMethods{..} -> do
          res <- amSimpleFilter [#isLegendary ?/~ True]
          map isbn res `shouldBe` [Isbn 1, Isbn 2]

    describe "Regex filters (dummy implementation)" $ do

      it "Contains match" $
        \ApiMethods{..} -> do
          do
            res <- amSimpleFilter [#name `textContains` "tofu"]
            map isbn res `shouldBe` [Isbn 2]

          do
            res <- amSimpleFilter [#name `textContains` "h"]
            map isbn res `shouldBe` [Isbn 2, Isbn 3]

      it "Contains case-insensitive match" $
        \ApiMethods{..} -> do
          do
            res <- amSimpleFilter [#name `textIContains` "b"]
            map isbn res `shouldBe` [Isbn 2, Isbn 3]

          do
            res <- amSimpleFilter [#name `textIContains` "B"]
            map isbn res `shouldBe` [Isbn 2, Isbn 3]

      it "Like: basic behaviour" $
        \ApiMethods{..} -> do
          res <- amSimpleFilter [#name `textLike` "*ook *"]
          map isbn res `shouldBe` [Isbn 2, Isbn 3]

      it "Like: sticks to edges by default" $
        \ApiMethods{..} -> do
          res <- amSimpleFilter [#name `textLike` "*g"]
          map isbn res `shouldBe` [Isbn 3]
          -- ↑ May also return Isbn 2 since it contains @g@ in the middle,
          -- but that would be not according to the spec.
