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

instance ( HasLoggingServer config lcontext subApi ctx
         , HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters
         , ReifySortingItems base
         , ReifyParamsNames provided
         ) =>
         HasLoggingServer config lcontext (SortingParams provided base :> subApi) ctx where
    routeWithLog =
        inRouteServer @(SortingParams provided base :> LoggingApiRec config lcontext subApi) route $
        \(paramsInfo, handler) sorting@(SortingSpec provided) ->
            let paramLog
                  | null provided = "no sorting"
                  | otherwise = fmt . mconcat $
                                "sorting: " : L.intersperse " " (map build provided)
            in (addParamLogInfo paramLog paramsInfo, handler sorting)
