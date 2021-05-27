{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Util.Combinators.Sorting.Client () where

import Universum

import qualified Data.Text as T
import Servant.API (ToHttpApiData (..), (:>))
import Servant.Client.Core (Client, HasClient (..))
import Servant.Server (Tagged (..))

import Servant.Util.Combinators.Sorting.Base


instance HasClient m subApi =>
         HasClient m (SortingParams provided base :> subApi) where
    type Client m (SortingParams provided base :> subApi) =
        SortingSpec provided base -> Client m subApi
    clientWithRoute mp _ req (SortingSpec sorting) =
        clientWithRoute mp (Proxy @(SortParamsExpanded provided subApi)) req
            (Just $ Tagged sorting)
    hoistClientMonad mp _ hst subCli = hoistClientMonad mp (Proxy @subApi) hst . subCli

instance ToHttpApiData (TaggedSortingItemsList provided) where
    toUrlPiece (Tagged sorting) =
        T.intercalate "," $ sorting <&> \SortingItem{..} ->
             let order = case siOrder of
                     Ascendant  -> "asc"
                     Descendant -> "desc"
             in order <> "(" <> siName <> ")"
