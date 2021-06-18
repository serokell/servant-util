{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Books where

import Universum

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Swagger (Swagger, ToParamSchema, ToSchema)
import Fmt (Buildable (..), (+|), (|+))
import qualified Network.Wai.Handler.Warp as Warp
import Servant (FromHttpApiData, Get, JSON, PostCreated, QueryParam, ReqBody, Server, serve,
                (:<|>) (..), (:>))
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

import Servant.Util

newtype Isbn = Isbn Word64
    deriving stock (Eq, Show, Generic)
    deriving newtype (ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData)

type instance SupportedFilters Isbn = '[FilterMatching, FilterComparing]

type instance ParamDescription Isbn = "ISBN of a book"
type instance ParamDescription Text = "Text"

data Book = Book
    { isbn     :: Isbn
    , bookName :: Text
    , author   :: Text
    }
  deriving (Generic)
  deriving anyclass (ToSchema)

deriveJSON defaultOptions 'Book

newtype Password = Password Text
  deriving stock (Generic)
  deriving newtype (FromHttpApiData, ToParamSchema)

type GetBooks
    =  SortingParams
         '["isbn" ?: Isbn, "name" ?: Text, "author" ?: Text]
         '["isbn" ?: 'Asc Isbn]
    :> FilteringParams ["isbn" ?: 'AutoFilter Isbn, "name" ?: 'AutoFilter Text]
    :> PaginationParams ('DefPageSize 20)
    :> Get '[JSON] [Book]

type AddBook
    =  QueryParam "password" Password
    :> ReqBody '[JSON] Book
    :> PostCreated '[JSON] Isbn

type BooksAPI = "books" :> (
    GetBooks :<|>
    AddBook
  )

swagger :: Swagger
swagger = toSwagger @BooksAPI Proxy

booksHandlers :: Server BooksAPI
booksHandlers =
    (\_sorting _filtering _pagination -> return [])
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
    serverWithLogging loggingConfig (Proxy @BooksAPI) $ \(Proxy :: Proxy api) ->
    serve @(SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> api) Proxy
      (swaggerSchemaUIServer swagger :<|> booksHandlers)
  where
    loggingConfig = ServantLogConfig putTextLn
