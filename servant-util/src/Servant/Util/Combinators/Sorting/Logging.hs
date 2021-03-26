{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Util.Combinators.Sorting.Logging () where

import Universum

import qualified Data.List as L
import Fmt (Buildable (..), fmt)
import Servant.API ((:>))
import Servant.Server (DefaultErrorFormatters, ErrorFormatters, HasContextEntry, HasServer (..))

import Servant.Server.Internal.Context (type (.++))
import Servant.Util.Combinators.Logging
import Servant.Util.Combinators.Sorting.Base
import Servant.Util.Combinators.Sorting.Server ()
import Servant.Util.Common

instance ( HasLoggingServer config subApi ctx
         , HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters
         , ReifyParamsNames params
         ) =>
         HasLoggingServer config (SortingParams params :> subApi) ctx where
    routeWithLog =
        inRouteServer @(SortingParams params :> LoggingApiRec config subApi) route $
        \(paramsInfo, handler) sorting@(SortingSpec params) ->
            let paramLog
                  | null params = "no sorting"
                  | otherwise = fmt . mconcat $
                                "sorting: " : L.intersperse " " (map build params)
            in (addParamLogInfo paramLog paramsInfo, handler sorting)
