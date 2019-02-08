{-# LANGUAGE DeriveFunctor #-}

module Servant.Util.Combinators.Filtering.Filters
    ( FilterMatching (..)
    , FilterComparing (..)
    , FilterOnLikeTemplate (..)
    , LikeFormatter (..)

    , NumericFilterTypes
    , TextFilterTypes
    , DatetimeFilterTypes
    ) where

import Universum

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Fmt (build, listF, (+|), (|+))
import Servant (FromHttpApiData (..))
import System.Console.Pretty (Color (..), Style (..), color, style)

import Servant.Util.Combinators.Filtering.Base

-------------------------------------------------------------------------
-- Filter types
-------------------------------------------------------------------------

-- | Support for @(==)@, @(/=)@ and @IN <values list>@ operations.
data FilterMatching a
    = FilterMatching a
    | FilterNotMatching a
    | FilterItemsIn [a]
    deriving (Functor)

instance BuildableAutoFilter FilterMatching where
    buildAutoFilter name = \case
        FilterMatching v -> build name <> " = " <> build v
        FilterNotMatching v -> build name <> " /= " <> build v
        FilterItemsIn v -> build name <> " ∊ " <> listF v

instance IsAutoFilter FilterMatching where
    autoFilterParsers _ = M.fromList
        [ ( defFilteringCmd
          , FilterMatching <$> parseFilteringValueAsIs
          )
        , ( "neq"
          , FilterNotMatching <$> parseFilteringValueAsIs
          )
        , ( "in"
          , FilterItemsIn <$> FilteringValueParser parseValuesList
          )
        ]
      where
        parseValuesList text = do
            text' <- maybeToRight ("Expected comma-separated list within '[]'") $
                T.stripPrefix "[" text >>= T.stripSuffix "]"
            let vals = T.splitOn "," text'
            mapM parseUrlPiece vals

    mapAutoFilterValue = fmap


-- | Support for @(<)@, @(>)@, @(<=)@ and @(>=)@ operations.
data FilterComparing a
    = FilterGT a
    | FilterLT a
    | FilterGTE a
    | FilterLTE a
    deriving (Functor)

instance BuildableAutoFilter FilterComparing where
    buildAutoFilter name = \case
        FilterGT v -> build name <> " > " <> build v
        FilterLT v -> build name <> " < " <> build v
        FilterGTE v -> build name <> " >= " <> build v
        FilterLTE v -> build name <> " <= " <> build v

instance IsAutoFilter FilterComparing where
    autoFilterParsers _ = M.fromList
        [ ( "gt"
          , FilterGT <$> parseFilteringValueAsIs
          )
        , ( "lt"
          , FilterLT <$> parseFilteringValueAsIs
          )
        , ( "gte"
          , FilterGTE <$> parseFilteringValueAsIs
          )
        , ( "lte"
          , FilterLTE <$> parseFilteringValueAsIs
          )
        ]

    mapAutoFilterValue = fmap


-- | SQL like pattern.
newtype LikeFormatter = LikeFormatter { unLikeFormatter :: Text }

instance Buildable LikeFormatter where
    build (LikeFormatter f) = like |+ " " +| f |+ ""
      where
        like = style Faint (color White "like") :: Text

-- | Support for SQL's LIKE syntax.
data FilterOnLikeTemplate a
    = FilterOnLikeTemplate LikeFormatter
    deriving (Functor)

instance BuildableAutoFilter FilterOnLikeTemplate where
    buildAutoFilter name = \case
        FilterOnLikeTemplate f -> build name <> " " <> build f

-- instance IsAutoFilter FilterOnLikeTemplate where
--     autoFilterParsers _ = M.fromList
--         [ ( "like"
--           , undefined

--           )
--         ]

-------------------------------------------------------------------------
-- Basic filters support
-------------------------------------------------------------------------

type NumericFilterTypes = [FilterMatching, FilterComparing]
type TextFilterTypes = [FilterMatching, FilterComparing]
type DatetimeFilterTypes = '[FilterComparing]

type instance SupportedFilters Bool = '[FilterMatching]
type instance SupportedFilters Int = NumericFilterTypes
type instance SupportedFilters Text = TextFilterTypes
type instance SupportedFilters ByteString = TextFilterTypes
type instance SupportedFilters UTCTime = DatetimeFilterTypes
