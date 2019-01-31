# Servant-util-beam-pg

This package contains backend implementation for API combinators of `servant-util` package via 'beam-postgres'.

## Build Instructions [↑](#-patak)

Run `stack build servant-util-beam-pg` to build everything.

## Usage [↑](#-patak)

### Sorting

We will demonstrate this on the [previously shown example](/servant-util/README.md#sorting):

```haskell

type instance SortingParamTypesOf Book =
    ["isbn" ?: Word64, "name" ?: Text]

type GetBooks
    :> SortingParamsOf Book
    =  Get '[JSON] [Book]

```

Let's assume that `Book` definition is extended to a Beam table (this is not necessary,
but simplifies our example):

```haskell
data BookT f = Book
    { isbn :: C f Isbn
    , bookName :: C f Text
    , author :: C f Text
    } deriving (Generic)

type Book = BookT Identity
```

Implementation will look like

```haskell

import Servant.Util (SortingSpecOf)
import Servant.Util.Beam.Postgres (bySpec_, fieldSort_)

-- some code

getBooks :: _ => SortingSpecOf Book -> m [Book]
getBooks sorting =
    runInsert . select $
        orderBy_ (bySpec_ sorting . sortingApp) $
        all_ (books booksSchema)
  where
    sortingApp Book{..} =
        fieldSort_ @"isbn" isbn .*.
        fieldSort_ @"name" bookName .*.
        HNil
```

Function `sortingApp` specifies how to correlate user-provided specification with fields
of our table. For each field which we allow to sort on we specify a Beam field from the
table.

If one of the fields lacks such specification in `sortingApp` definition or order of
fields is incorrect then compile error is raised. The same happens when field types in API
and schema definition mismatch.

Annotating `fieldSort_` calls with a field name is fully optional but may save you in case
when several fields of the same type participate in sorting.

### Pagination

Pagination can be appled simply with `paginate_` function.

## For Contributors [↑](#-patak)

Please see [CONTRIBUTING.md](/.github/CONTRIBUTING.md) for more information.

## About Serokell [↑](#-patak)

Servant-util is maintained and funded with :heart: by [Serokell](https://serokell.io/). The names and logo for Serokell are trademark of Serokell OÜ.

We love open source software! See [our other projects](https://serokell.io/community?utm_source=github) or [hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow your idea!
