module Books where

import Universum

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Fmt ((+|), (|+))
import Fmt (Buildable (..))
import qualified Network.Wai.Handler.Warp as Warp
import Servant ((:<|>) (..), (:>), FromHttpApiData, Get, JSON, PostCreated, QueryParam, ReqBody,
                Server, serve)

import Servant.Util

newtype Isbn = Isbn Word64
    deriving (Eq, Show, ToJSON, FromJSON)

data Book = Book
    { isbn     :: Isbn
    , bookName :: Text
    , author   :: Text
    }

deriveJSON defaultOptions 'Book

newtype Password = Password Text
    deriving (FromHttpApiData)

type GetBooks
    = Get '[JSON] [Book]

type AddBook
    =  QueryParam "password" Password
    :> ReqBody '[JSON] Book
    :> PostCreated '[JSON] Isbn

type BooksAPI = "books" :> (
    GetBooks :<|>
    AddBook
  )

booksHandlers :: Server BooksAPI
booksHandlers =
    (return [])
    :<|>
    (\_ book -> return (isbn book))

warpSettings :: Warp.Settings
warpSettings = Warp.defaultSettings
    & Warp.setHost "127.0.0.1"
    & Warp.setPort 8090

instance Buildable Isbn where
    build (Isbn i) = "isbn:" <> build i

instance Buildable Password where
    build _ = "<password>"

instance Buildable Book where
    build Book{..} =
        "{ isbn = " +| isbn |+
        ", title = " +| bookName |+
        ", author = " +| author |+
        "}"

instance Buildable (ForResponseLog Isbn) where
    build = buildForResponse

instance Buildable (ForResponseLog Book) where
    build = buildForResponse

instance Buildable (ForResponseLog [Book]) where
    build = buildListForResponse (take 5)

serveBooksServer :: IO ()
serveBooksServer =
    Warp.runSettings warpSettings $
    serverWithLogging loggingConfig (Proxy @BooksAPI) $ \sp ->
    serve sp booksHandlers
  where
    loggingConfig = ServantLogConfig putTextLn
