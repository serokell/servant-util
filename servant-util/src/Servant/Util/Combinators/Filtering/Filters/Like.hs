{-# LANGUAGE DeriveFunctor #-}

module Servant.Util.Combinators.Filtering.Filters.Like
    ( pattern Esc
    , mkLikePattern
    , LikePattern (..)
    , FilterLike (..)
    ) where

import Universum

import qualified Data.Map as M
import qualified Data.Text.Lazy as LT
import Fmt (build, (+|), (|+))
import Servant (ToHttpApiData (..), FromHttpApiData (..))
import System.Console.Pretty (Color (..), Style (..), color, style)

import Servant.Util.Combinators.Filtering.Base

-- | Whether search is case-sensitive.
newtype CaseSensitivity = CaseSensitivity Bool

instance Buildable CaseSensitivity where
    build (CaseSensitivity cs)
        | cs = ""
        | otherwise = "(case-insensitive)"

-- | Simple regexp pattern, @.@ and @*@ signed will be considered.
-- Escaping is performed via prefixing with backslash.
data LikePattern = LikePatternUnsafe
    { unLikePattern :: LText
    }

pattern Esc :: Char
pattern Esc = '\\'

escapedChar :: Char -> LText
escapedChar c = LT.pack [Esc, c]

mkLikePattern :: LText -> Either Text LikePattern
mkLikePattern txt = do
    if valid (toString txt)
        then pass
        else Left "Single escape character ('\') is not allowed"
    return (LikePatternUnsafe txt)
  where
    valid = \case
        Esc : Esc : r -> valid r
        Esc : '.' : r -> valid r
        Esc : '*' : r -> valid r
        Esc : _       -> False
        _ : r         -> valid r
        []            -> True

instance IsString LikePattern where
    fromString = either error id . mkLikePattern . fromString

instance Buildable LikePattern where
    build (LikePatternUnsafe p) = like |+ " " +| p |+ ""
      where
        like = style Faint (color White "like") :: Text

-- | Support for SQL's LIKE syntax.
data FilterLike a
    = FilterLike CaseSensitivity LikePattern
    deriving (Functor)

instance BuildableAutoFilter FilterLike where
    buildAutoFilter name = \case
        FilterLike cs f -> build name <> " " <> build f <> " " <> build cs

instance IsAutoFilter FilterLike where
    autoFilterEnglishOpsNames =
        [ ("like", "like pattern match")
        , ("ilike", "case-insensitive like pattern match")
        , ("contains", "contains text")
        , ("icontains", "case-insensitive 'contains text'")
        ]

    autoFilterParsers _ = M.fromList
        [ ( "like"
          , FilterLike (CaseSensitivity True) <$> parseLikePattern
          )
        , ( "ilike"
          , FilterLike (CaseSensitivity False) <$> unsupportedFilteringValue
              "Case-insensitive filters are not supported by this backend."
          )
        , ( "contains"
          , FilterLike (CaseSensitivity True) <$> containsPattern
          )
        , ( "icontains"
          , FilterLike (CaseSensitivity False) <$> unsupportedFilteringValue
              "Case-insensitive filters are not supported by this backend."
          )
        ]
      where
        parseLikePattern = FilteringValueParser $ \t -> do
            pat <- parseUrlPiece t
            mkLikePattern pat

        containsPattern = do
            LikePatternUnsafe . asContains <$> parseFilteringValueAsIs
        asContains t = t
            & LT.replace "." (escapedChar '.')
            & LT.replace "*" (escapedChar '*')
            & LT.replace (LT.singleton Esc) (escapedChar Esc)
            & LT.cons '*'
            & flip LT.snoc '*'

    autoFilterEncode = \case
        FilterLike cs (unLikePattern -> pat)
            | CaseSensitivity True <- cs
                -> ("like", toQueryParam pat)
            | otherwise
                -> ("ilike", toQueryParam pat)
