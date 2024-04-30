{-# LANGUAGE OverloadedLists #-}

module Test.Servant.Sorting.ImplSpec where

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
    { isbn = Isbn 111
    , name = "How to cook tofu, the right way"
    , rating = 5
    , createdAt = toUTCTime 1
    }

  , Book
    { isbn = Isbn 100
    , name = "Delving into memes"
    , rating = 5
    , createdAt = toUTCTime 3
    }

  , Book
    { isbn = Isbn 120
    , name = "Book of something"
    , rating = 4
    , createdAt = toUTCTime 8
    }

  ]

-- Server
---------------------------------------------------------------------------

data ApiMethods route = ApiMethods
  { amSimpleSort
      :: route
      :- "simple"
      :> SortingParams
          [ "name" ?: Text
          , "rating" ?: Word8
          , "created_at" ?: UTCTime
          ]
         '[ "isbn" ?: 'Asc Isbn
          ]
      :> Get '[JSON] [Book]

  , amOverrideableSort
      :: route
      :- "overrideable"
      :> SortingParams
          [ "rating" ?: Word8
          , "created_at" ?: UTCTime
          , "isbn" ?: Isbn
          ]
          [ "rating" ?: 'Desc Word8
          , "isbn" ?: 'Asc Isbn
          ]
      :> Get '[JSON] [Book]

  , amOverrideableSort2
      :: route
      :- "overrideable2"
      :> SortingParams
          [ "isbn" ?: Isbn
          , "rating" ?: Word8
          ]
          [ "isbn" ?: 'Asc Isbn
          , "rating" ?: 'Asc Word8
          ]
      :> Get '[JSON] [Book]

  } deriving Generic

apiHandlers :: ApiMethods AsServer
apiHandlers = ApiMethods
  { amSimpleSort = \sortingSpec -> do
      let sortingApp Book{..} =
            fieldSort @"name" name .*.
            fieldSort @"rating" rating .*.
            fieldSort @"created_at" createdAt .*.
            fieldSort @"isbn" isbn .*.
            HNil
      return $ sortBySpec sortingSpec sortingApp allBooks

  , amOverrideableSort = \sortingSpec -> do
      let sortingApp Book{..} =
            fieldSort @"rating" rating .*.
            fieldSort @"created_at" createdAt .*.
            fieldSort @"isbn" isbn .*.
            fieldSort @"rating" rating .*.
            fieldSort @"isbn" isbn .*.
            HNil
      return $ sortBySpec sortingSpec sortingApp allBooks

  , amOverrideableSort2 = \sortingSpec -> do
      let sortingApp Book{..} =
            fieldSort @"isbn" isbn .*.
            fieldSort @"rating" rating .*.
            fieldSort @"isbn" isbn .*.
            fieldSort @"rating" rating .*.
            HNil
      return $ sortBySpec sortingSpec sortingApp allBooks

  }

-- Swagger
---------------------------------------------------------------------------

-- You can try this with ghci
printSortingSwagger :: IO ()
printSortingSwagger =
  writeFile "sorting-test-swagger.json" $
    decodeUtf8 . Aeson.encode $
    toSwagger $ Proxy @(ToServantApi ApiMethods)

-- Tests
---------------------------------------------------------------------------

spec :: Spec
spec =
  aroundAll (runTestServer apiHandlers) $ do

    describe "Simple sort with base sorting" $ do

      it "No sorting (sole base)" $
        \ApiMethods{..} -> do
          res <- amSimpleSort noSorting
          map isbn res `shouldBe` [Isbn 100, Isbn 111, Isbn 120]

      it "Fully determining sorting (base doesn't matter)" $
        \ApiMethods{..} -> do
          res <- amSimpleSort [asc #created_at]
          map isbn res `shouldBe` [Isbn 111, Isbn 100, Isbn 120]

      it "Partially determining sorting (base matters)" $
        \ApiMethods{..} -> do
          res <- amSimpleSort [asc #rating]
          map isbn res `shouldBe` [Isbn 120, Isbn 100, Isbn 111]

    describe "Simple sort with overridable base sorting" $ do

      it "No sorting (sole base)" $
        \ApiMethods{..} -> do
          res <- amOverrideableSort noSorting
          map isbn res `shouldBe` [Isbn 100, Isbn 111, Isbn 120]

      it "Base sorting can be overriden" $
        \ApiMethods{..} -> do
          res <- amOverrideableSort [desc #isbn]
          map isbn res `shouldBe` [Isbn 120, Isbn 111, Isbn 100]

      it "Overriden base sorting tolerates the provided sorting order" $
        -- One possible bug is on @rating+,createdAt+@ request ignore @rating+@
        -- since it is part of base sorting. But base sorting matters last, so
        -- that wouldn't be valid.
        \ApiMethods{..} -> do
          res <- amOverrideableSort [asc #rating, asc #created_at]
          map isbn res `shouldBe` [Isbn 120, Isbn 111, Isbn 100]

      it "Overriden field from base sorting doesn't cancel the remaining base sorting" $
        \ApiMethods{..} -> do
          res <- amOverrideableSort2 [asc #rating]
          map isbn res `shouldBe` [Isbn 120, Isbn 100, Isbn 111]
