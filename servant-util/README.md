# Servant-util

This package contains the core primitives which directly participate in API and some common utilities.

## Build Instructions [↑](#-patak)

Run `stack build servant-util` to build everything.

## Usage [↑](#-patak)

For the following examples we consider a simple server with two endpoints.

```haskell

import Universum

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant ((:<|>) (..), (:>), FromHttpApiData, Get, JSON, PostCreated, QueryParam, ReqBody,
                Server, serve)
import Servant.Util ()

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
booksHandlers = -- error "To be implemented"
    (return [])
    :<|>
    (\_ book -> return (isbn book))

warpSettings :: Warp.Settings
warpSettings = Warp.defaultSettings
    & Warp.setHost "127.0.0.1"
    & Warp.setPort 8090

serveBooksServer :: IO ()
serveBooksServer =
    Warp.runSettings warpSettings $
    serve (Proxy @BooksAPI) booksHandlers

```

### Sorting

When an endpoint is extended with `SortingParams` combinator it starts to accept a sorting
specification in `sortBy` query parameter. This way the user can supply a sequence of fields
from the set of allowed fields, they will be applied in lexicographical sorting.

For example, `GetBooks` from the example above can be extended as

```haskell

type GetBooks
    :> SortingParams ["isbn" ?: Word64, "name" ?: Text]
    =  Get '[JSON] [Book]

```

List required by `SortingParams` combinator should consist of fields which are allowed to participate
in sorting.
The first argument of `?:` operator stands for a field name from front-end's point
of view; the second argument corresponds to field type and used primarily to avoid
mistakes in implementation.

(Soon it will also be used to distinguish between nullable and mandatory fields.)

Examples of valid requests to this server (using [httpie](https://httpie.org/)):
* `http :8090/books sortBy=='asc(name)'`
* `http :8090/books sortBy=='asc(name),desc(isbn)'`
* `http :8090/books sortBy==+name,-isbn'`

Server handler will be supplied with `SortingSpec` argument, use neighbor `servant-util-*`
packages for applying this specification to your backend.

Since a list of fields which can participate in sorting is usually determined by response
type, you can extract it to instance of the dedicated type family helper:

```haskell

type instance SortingParamTypesOf Book =
    ["isbn" ?: Word64, "name" ?: Text]

type GetBooks
    :> SortingParamsOf Book  -- same as `SortingParams (SortingParamTypesOf Book)`
    =  Get '[JSON] [Book]

```

### Pagination

Pagination is applied via `PaginationParams` combinator. It accepts a settings type argument,
which is currently just `DefPageSize n` type; this is required in order for default page
size to be reflected in documentation.

Endpoint supplied with this combinator starts to accept `offset` and `limit` query
parameters, both are optional.

Your endpoint implementation will be given a `PaginationSpec` object which can be applied
with an appropriate function.

```haskell

import Servant.Util (PaginationParams, PaginationSpec)
import Servant.Util.Dummy (paginate)

type GetBooks
    :> PaginationParams (DefPageSize 10)
    =  Get '[JSON] [Book]

getBooks :: _ => PaginationSpec -> m [Books]
getBooks pagination = paginate pagination <$> getAllBooks

```

### Logging

#### Problem

One can enable logging of incoming requests in a very simple way using [`logStdoutDev`](http://hackage.haskell.org/package/wai-extra-3.0.25/docs/Network-Wai-Middleware-RequestLogger.html#v:logStdoutDev)
function from `wai-extra` package.

Let's try it:
```sh
http POST :8090/books isbn:=12312 bookName='Some book' author=unknown password==qwerty123
```

Produced log:
```
POST /books
  Params: [("password","qwerty123")]
  Request Body: {"isbn": 12312, "bookName": "Some book", "author": "unknown"}
  Accept: application/json, */*
  Status: 201 Created 0.000086s
```

Note the problem: logs contain the password which makes them unusable in production.

And if we use
[`logStdout`](http://hackage.haskell.org/package/wai-extra-3.0.25/docs/Network-Wai-Middleware-RequestLogger.html#v:logStdout),
logs are missing the most part of the request data:

```
127.0.0.1 - - [29/Jan/2019:02:32:31 +0300] "POST /books?password=qwerty123 HTTP/1.1" 201 -
"" "HTTPie/0.9.8"
```

#### Proposed solution

A reasonable way to resolve this would be to display objects depending on their semantics.
This package provides an implementation of such logging, it can be used as follows:

```haskell

serveBooksServer :: IO ()
serveBooksServer =
    Warp.runSettings warpSettings $
    serverWithLogging loggingConfig (Proxy @BooksAPI) $ \sp ->
    serve sp booksHandlers
  where
    loggingConfig = ServantLogConfig putTextLn

```

This will wrap your API into internal `LoggingAPI` combinator, resulting API `Proxy`
should be passed to `serve` method; meanwhile, handlers remain unchanged.

You will also need to provide `Buildable` instances for all request parameters:

```haskell

instance Buildable Isbn where
    build (Isbn i) = "isbn:" <> build i

instance Buildable Password where
    build _ = "<password>"

instance Buildable Book where
    build Book{..} =
        "{ isbn = " +| isbn |+
        ", title = " +| bookName |+
        ", author = " +| author |+
        " }"

```

and `instance Buildable (ForResponseLog *)` for all types appearing as a response.
Note that semantics of `ForResponseLog` newtype wrapper is displaying a reasonable part of
a response: not too little to stay informative, not too large in order to keep logs small.

```haskell

instance Buildable (ForResponseLog Isbn) where
    build = buildForResponse

instance Buildable (ForResponseLog Book) where
    build = buildForResponse

instance Buildable (ForResponseLog [Book]) where
    build = buildListForResponse (take 5)

```

Now logs look like

```

POST Request #1
    :> books
    :> 'password' field: <password>
    :> request body: { isbn = isbn:12312, title = Some book, author = unknown }

    Response #1 OK 0.013415s > isbn:12312

```

## For Contributors [↑](#-patak)

Please see [CONTRIBUTING.md](/.github/CONTRIBUTING.md) for more information.

## About Serokell [↑](#-patak)

Servant-util is maintained and funded with :heart: by [Serokell](https://serokell.io/). The names and logo for Serokell are trademark of Serokell OÜ.

We love open source software! See [our other projects](https://serokell.io/community?utm_source=github) or [hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow your idea!
