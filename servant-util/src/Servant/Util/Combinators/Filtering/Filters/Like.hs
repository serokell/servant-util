{-# LANGUAGE DeriveFunctor #-}

module Servant.Util.Combinators.Filtering.Filters.Like
    ( pattern Esc
    , CaseSensitivity (..)
    , mkLikePattern
    , LikePattern (..)
    , FilterLike (..)
    , mkLikePatternUnsafe
    , filterContains
    ) where

import Universum

import qualified Data.Map as M
import qualified Data.Text.Lazy as LT
import Fmt (Buildable (..), (+|), (|+))
import Servant (FromHttpApiData (..), ToHttpApiData (..))
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
newtype LikePattern = LikePatternUnsafe
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

mkLikePatternUnsafe :: LText -> LikePattern
mkLikePatternUnsafe = either error id . mkLikePattern

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

-- | Construct a filter that matches when text contains given substring.
filterContains :: CaseSensitivity -> Text -> FilterLike a
filterContains cs pat =
    FilterLike cs (LikePatternUnsafe $ asContains $ LT.fromStrict pat)
  where
    asContains t = t
        & LT.replace "." (escapedChar '.')
        & LT.replace "*" (escapedChar '*')
        & LT.replace (LT.singleton Esc) (escapedChar Esc)
        & LT.cons '*'
        & flip LT.snoc '*'

instance BuildableAutoFilter FilterLike where
    buildAutoFilter name = \case
        FilterLike cs f -> build name <> " " <> build f <> " " <> build cs

instance IsAutoFilter FilterLike where
    autoFilterEnglishOpsNames =
        [ ("like", "regex match (`.` for any char, `*` for any substring)")
        , ("ilike", "case-insensitive regex")
        , ("contains", "contains text, requires no escaping")
        , ("icontains", "case-insensitive 'contains text'")
        ]

    autoFilterParsers _ = M.fromList
        [ ( "like"
          , FilterLike (CaseSensitivity True) <$> parseLikePattern
          )
        , ( "ilike"
          , FilterLike (CaseSensitivity False) <$> parseLikePattern
          )
        , ( "contains"
          , filterContains (CaseSensitivity True) <$> parseFilteringValueAsIs
          )
        , ( "icontains"
          , filterContains (CaseSensitivity False) <$> parseFilteringValueAsIs
          )
        ]
      where
        parseLikePattern = FilteringValueParser $ \t -> do
            pat <- parseUrlPiece t
            mkLikePattern pat

    autoFilterEncode = \case
        FilterLike cs (unLikePattern -> pat)
            | CaseSensitivity True <- cs
                -> ("like", toQueryParam pat)
            | otherwise
                -> ("ilike", toQueryParam pat)
