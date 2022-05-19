# Servant-util

This package contains the core primitives which directly participate in API and some common utilities.

## Build Instructions

Run `stack build servant-util` to build everything.

## Usage

For the following examples, we consider a simple server with two endpoints.

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
booksHandlers =
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

When an endpoint is extended with a `SortingParams` combinator, it starts to accept a sorting
specification in `sortBy` query parameter. This way, the user can supply a sequence of fields
from the set of allowed fields. They will be applied lexicographically in the specified order.

For example, `GetBooks` from the example above can be extended as

```haskell

type GetBooks
    =  SortingParams
         ["isbn" ?: Isbn, "name" ?: Text, "author" ?: Text]
        '["isbn" ?: 'Asc Isbn]
    :> Get '[JSON] [Book]

```

The first list required by `SortingParams` combinator should consist of fields that
are allowed to participate in sorting.
The first argument of `?:` operator stands for a field name from front-end's point
of view; the second argument corresponds to the field type and used primarily to avoid
mistakes in the implementation.

(Soon, it will also be used to distinguish between nullable and mandatory fields.)

The second list required by `SortingParams` stands for the base sorting that will
always be applied last disregard the user's input. It allows for more
deterministic results and is, in fact, essential when paired with pagination.

Examples of valid requests to this server (using [httpie](https://httpie.org/)):
* `http :8090/books sortBy=='asc(name)'` — sort alphabetically by `name` (for equal names — by `isbn`);
* `http :8090/books sortBy=='asc(name),desc(author)'` — sort alphabetically by `name`, for equal names — by `author` in reversed order (and for entries with the same name and author - by `isbn`).
* `http :8090/books sortBy==+name,-author'` — same as above.

The server handler will be supplied with `SortingSpec` argument, use neighbor `servant-util-*`
packages for applying this specification to your backend.

You can also use methods from [`Servant.Util.Dummy`](src/Servant/Util/Dummy.hs) for a trivial in-Haskell
implementation, suitable for a server prototype:

```haskell

import Servant.Util.Dummy (sortBySpec, fieldSort)

getBooks :: ToServer GetBooks
getBooks sortingSpec = do
    let
      -- Correlate user input with fields of our response type
      sortingApp Book{..} =
          fieldSort @"name" bookName .*.
          fieldSort @"author" author .*.
          HNil
    sortBySpec sortingSpec sortingApp <$> allBooks
```

Since a list of fields that can participate in sorting is usually determined by the response
type, you can extract this fields list to an instance of the dedicated type family helper:

```haskell

type instance SortingParamBaseOf Book =
    ["name" ?: Isbn, "author" ?: Text]
type instance SortingParamProvidedOf Book =
    ["isbn" ?: Asc Isbn]

type GetBooks
    =  SortingParamsOf Book  -- same as the definition at the section's top
    :> Get '[JSON] [Book]

```

In case you need to construct a `SortingSpec` manually (for instance, to pass to a client handler),
take a look at [`Servant.Util.Combinators.Sorting.Construction`](src/Servant/Util/Combinators/Sorting/Construction.hs) module.


### Filtering

This package provides support for many types of filtering: exact matching, comparisons,
text search; these are called automatic filters. Complex filtering conditions which do
not (and cannot) fall into one of the mentioned categories are also allowed and here they
are called manual filters. When user specifies multiple filters, their conjunction is
applied.

Let's consider filters on the previous example. Your API should be extended with
`FilteringParams` combinator which is pretty similar to `SortingParams`.

```haskell

type GetBooks
    =  FilteringParams ["isbn" ?: 'AutoFilter Isbn, "name" ?: 'AutoFilter Text]
    :> Get '[JSON] [Book]

```

Your endpoint implementation will be provided with `FilteringSpec` argument. Note that
this time parameters list contains not only parameter name and type, but also the type of
filter.

Just like for sorting, here you can use `SortingParamTypesOf` type family to reduce the
boilerplate.

Now you need to tell which automatic filter types are allowed for your types:

```haskell

type SupportedFilters Isbn = '[FilterMatching, FilterComparing]
type SupportedFilters Text = '[FilterMatching, FilterComparing]
-- the latter is already defined in this library

```

On the frontend side the following query parameters will be allowed:

* `isbn=12345` - plain matching filter.
* `isbn[eq]=12345` - same as above.
* `isbn[neq]=12345` - values not equal to the given one.
* `isbn[in]=[12345,23456]` - any value from the given list matches.
* `isbn[gt]=12345` - higher values are allowed.
* `isbn[lte]=12345` - the opposite to the previous predicate.
* `name[gte]=D&name[lt]=E` - everything starting with latter `D`.

Now let's suppose you need your server backend to support a much more complex
predicate: for instance, the book's name is longer than `10` characters. You can either define
your own filter or use a manual one; let's demonstrate the latter case:

```haskell

type GetBooks
    =  FilteringParams ["hasLongName" ?: ManualFilter Bool]
    :> Get '[JSON] [Book]

```

This way user can supply only `hasLongName=false` or `hasLongName=true` query parameter,
but the filter implementation can be arbitrarily complex.

In case we put `FilteringParams` with all the fields mentioned above into our `GET` method,
a dummy implementation of its handler may look like:

```haskell

getBooks :: ToServer GetBooks
getBooks filterSpec = do
    let
      filterApp Book{..} =
        filterOn @"isbn" isbn .*.  -- automatic fields require only the field
        filterOn @"name" bookName .*.
        manualFilter @"hasLongName"  -- manual filter requires a predicate
            (\needLongName -> (length bookName >= 10) == needLongName) .*.
        HNil

    filterBySpec filterSpec filterApp <$> getAllBooks

```

If for any reason you need to construct a `FilteringSpec` manually, take a look at
[`Servant.Util.Combinators.Filtering.Construction`](src/Servant/Util/Combinators/Filtering/Construction.hs) module.

### Pagination

Pagination is applied via `PaginationParams` combinator. It accepts a `settings` type argument,
which is currently just either `'DefPageSize n` or `'DefUnlimitedPageSize` that define
the default page size (defined statically in order to be reflected in documentation).

An endpoint supplied with this combinator starts accepting `offset` and `limit` query
parameters, both are optional.

Your endpoint implementation will be given a `PaginationSpec` object which can be applied
with an appropriate function.

```haskell

import Servant.Util (PaginationParams, PaginationSpec)
import Servant.Util.Dummy (paginate)

type GetBooks
    =  PaginationParams
    :> Get '[JSON] [Book]

getBooks :: ToServer GetBooks
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

This will wrap your API into an internal `LoggingAPI` combinator, resulting API `Proxy`
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

## For Contributors

Please see [CONTRIBUTING.md](/.github/CONTRIBUTING.md) for more information.

## About Serokell

Servant-util is maintained and funded with :heart: by [Serokell](https://serokell.io/). The names and logo for Serokell are trademark of Serokell OÜ.

We love open source software! See [our other projects](https://serokell.io/community?utm_source=github) or [hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow your idea!
