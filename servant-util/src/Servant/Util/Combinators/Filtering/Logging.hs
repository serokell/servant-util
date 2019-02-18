{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Util.Combinators.Filtering.Logging
    ( BuildSomeFilter (..)
    ) where

import Universum

import qualified Data.List as L
import Data.Typeable (cast)
import Fmt (Buildable (..), Builder, fmt)
import Fmt ((+|), (|+))
import GHC.TypeLits (KnownSymbol)
import Servant ((:>), HasServer (..))

import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Combinators.Filtering.Server
import Servant.Util.Combinators.Logging
import Servant.Util.Common

-- | Print a filter as it should appear in logs.
class BuildSomeFilter params where
    buildSomeFilter' :: SomeFilter params -> Maybe Builder

instance BuildSomeFilter '[] where
    buildSomeFilter' _ = Nothing

instance ( KnownSymbol name
         , Typeable a
         , Buildable a
         , BuildSomeFilter params
         ) => BuildSomeFilter ('TyNamedParam name ('AutoFilter a) ': params) where
    buildSomeFilter' SomeFilter{..} = asum
        [ do
          guard (name == sfName)
          filtr :: TypeFilter 'AutoFilter a <- cast sfFilter
          return $ case filtr of TypeAutoFilter f -> build (name, f)

        , buildSomeFilter' @params SomeFilter{..}
        ]
      where
        name = symbolValT @name

instance ( KnownSymbol name
         , Typeable a
         , Buildable a
         , BuildSomeFilter params
         ) => BuildSomeFilter ('TyNamedParam name ('ManualFilter a) ': params) where
    buildSomeFilter' SomeFilter{..} = asum
        [ do
          guard (name == sfName)
          filtr :: TypeFilter 'ManualFilter a <- cast sfFilter
          return $ case filtr of TypeManualFilter v -> name |+ ": " +| v |+ ""

        , buildSomeFilter' @params SomeFilter{..}
        ]
      where
        name = symbolValT @name

buildSomeFilter :: BuildSomeFilter params => SomeFilter params -> Builder
buildSomeFilter sf = buildSomeFilter' sf ?: error "Failed to build some filter"

instance ( HasLoggingServer config subApi ctx
         , AreFilteringParams params
         , ReifyParamsNames params
         , BuildSomeFilter params
         ) =>
         HasLoggingServer config (FilteringParams params :> subApi) ctx where
    routeWithLog =
        inRouteServer @(FilteringParams params :> LoggingApiRec config subApi) route $
        \(paramsInfo, handler) filtering@(FilteringSpec params) ->
            let paramLog
                  | null params = "no filters"
                  | otherwise = fmt . mconcat $
                                L.intersperse ", " (map buildSomeFilter params)
            in (addParamLogInfo paramLog paramsInfo, handler filtering)
