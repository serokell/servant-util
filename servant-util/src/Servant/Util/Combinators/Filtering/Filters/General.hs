{-# LANGUAGE DeriveFunctor #-}

module Servant.Util.Combinators.Filtering.Filters.General
    ( FilterMatching (..)
    , FilterComparing (..)
    ) where

import Universum

import qualified Data.Map as M
import qualified Data.Text as T
import Fmt (build, listF)
import Servant (FromHttpApiData (..), ToHttpApiData (..))

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
        FilterMatching v    -> build name <> " = " <> build v
        FilterNotMatching v -> build name <> " /= " <> build v
        FilterItemsIn v     -> build name <> " ∊ " <> listF v

instance IsAutoFilter FilterMatching where
    autoFilterEnglishOpsNames =
        [ (DefFilteringCmd, "is equal to, _default operation_")
        , ("neq", "is not equal to")
        , ("in", "is one of")
        ]

    autoFilterParsers _ = M.fromList
        [ ( DefFilteringCmd
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

    autoFilterEncode = \case
        FilterMatching v    -> (DefFilteringCmd, toQueryParam v)
        FilterNotMatching v -> ("neq", toQueryParam v)
        FilterItemsIn vs    -> ("in", "[" <> T.intercalate "," (map toQueryParam vs) <> "]")


-- | Support for @(<)@, @(>)@, @(<=)@ and @(>=)@ operations.
data FilterComparing a
    = FilterGT a
    | FilterLT a
    | FilterGTE a
    | FilterLTE a
    deriving (Functor)

instance BuildableAutoFilter FilterComparing where
    buildAutoFilter name = \case
        FilterGT v  -> build name <> " > " <> build v
        FilterLT v  -> build name <> " < " <> build v
        FilterGTE v -> build name <> " >= " <> build v
        FilterLTE v -> build name <> " <= " <> build v

instance IsAutoFilter FilterComparing where
    autoFilterEnglishOpsNames =
        [ ("gt", "is greater")
        , ("lt", "is less")
        , ("gte", "is greater or equal")
        , ("lte", "is less or equal")
        ]

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

    autoFilterEncode = \case
        FilterGT v  -> ("gt", toQueryParam v)
        FilterLT v  -> ("lt", toQueryParam v)
        FilterGTE v -> ("gte", toQueryParam v)
        FilterLTE v -> ("lte", toQueryParam v)


-------------------------------------------------------------------------
-- Basic filters support
-------------------------------------------------------------------------
