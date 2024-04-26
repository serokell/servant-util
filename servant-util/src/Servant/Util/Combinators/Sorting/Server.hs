{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Util.Combinators.Sorting.Server () where

import Universum

import qualified Data.List as L
import qualified Data.Set as S
import Servant.API (FromHttpApiData (..), (:>))
import Servant.Server (DefaultErrorFormatters, ErrorFormatters, HasServer (..), Tagged (..),
                       unTagged)
import Servant.Server.Internal.Context (HasContextEntry, type (.++))
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import Servant.Util.Combinators.Sorting.Base
import Servant.Util.Common


-- | Ensure no name in entries repeat.
sortingCheckDuplicates :: [SortingItem] -> Either Text ()
sortingCheckDuplicates items =
    let names = map siName items
        duplicate = safeHead . mapMaybe (safeHead . drop 1) . L.group $ sort names
    in maybe pass (\n -> Left $ "Duplicated field " <> show n) duplicate

-- | Consumes "sortBy" query parameter and fetches sorting parameters contained in it.
instance ( HasServer subApi ctx
         , HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters
         , ReifySortingItems base
         , ReifyParamsNames provided
         ) =>
         HasServer (SortingParams provided base :> subApi) ctx where
    type ServerT (SortingParams provided base :> subApi) m =
        SortingSpec provided base -> ServerT subApi m

    route =
        inRouteServer @(SortParamsExpanded provided subApi) route $
        \handler rawSortItems -> handler (SortingSpec $ fmap unTagged rawSortItems ?: [])

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy @subApi) pc nt . s

-- | Parse 'sort_by' query param.
-- Following the format described in "Sorting" section of https://www.moesif.com/blog/technical/api-design/REST-API-Design-Filtering-Sorting-and-Pagination/
instance ReifyParamsNames allowed =>
         FromHttpApiData (TaggedSortingItemsList allowed) where
    parseUrlPiece =
        first (toText . P.errorBundlePretty) . second Tagged .
        P.parse parser "sortBy"
      where
        parser = do
            items <- P.sepBy itemParser (P.char ',')
            either (fail . toString) pure $ sortingCheckDuplicates items
            P.eof
            return items

        itemParser :: P.Parsec Void Text SortingItem
        itemParser = asum
            [ do
                siOrder <- asum
                    [ Ascendant <$ P.char '+'
                    , Descendant <$ P.char '-'
                    ] <?> "ordering sign (+/-)"
                siName <- paramNameParser
                return SortingItem{..}

            , do
                siOrder <- asum
                    [ Ascendant <$ P.string' "asc"
                    , Descendant <$ P.string' "desc"
                    ] <?> "ordering keyword (asc/desc)"
                siName <- P.char '(' *> paramNameParser <* P.char ')'
                return SortingItem{..}
            ]

        allowedParams = reifyParamsNamesSet @allowed

        paramNameParser = do
            -- The exlusion of `,` and `)` happens because 'paramNameParser' is called within 'itemParser',
            -- which itself does the exlusion of ')'. Since it hasn't happened yet, we need to be mindful of
            -- closing parens.
            name <- P.takeWhile1P (Just "sorting item name") (\c -> c /= ',' && c /= ')') <?> "parameter name"
            unless (name `S.member` allowedParams) $
                fail $ "unknown parameter " <> show name <>
                       " (expected one of " <> show (toList allowedParams) <> ")"
            return name
