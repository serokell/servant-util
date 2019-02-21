{-# LANGUAGE TypeInType #-}

-- | Filter getters.
--
-- Allows to get a value passed by user to a manual filter.
-- Extracing auto filters is not allowed as soon as they are too complex
-- to make anything useful with them anyway.
module Servant.Util.Combinators.Filtering.Getters
    ( manualFilterValue
    ) where

import Universum

import GHC.TypeLits (ErrorMessage (..), KnownSymbol, TypeError)

import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Common

type family UnManualFilter (fk :: FilterKind k) :: k where
    UnManualFilter ('ManualFilter a) = a
    UnManualFilter ('AutoFilter a) =
        TypeError ('Text "Getting an auto filter is not allowed")

-- | Extract a value from manual filter.
manualFilterValue
    :: forall name params filter filterKind.
       ( filterKind ~ LookupParam "filter" name (params :: [TyNamedFilter])
       , filter ~ UnManualFilter filterKind
       , Typeable filter
       , KnownSymbol name
       )
    => NameLabel name -> FilteringSpec params -> [filter]
manualFilterValue _ (FilteringSpec filters) =
    -- Probably this function should return @Maybe filter@ instead,
    -- but that requires some work (prohibiting specifying the same
    -- manual filter twice in server and in manual filters construction),
    -- and use cases for this getter are not clear yet.

    catMaybes $ filters <&> \SomeFilter{..} ->
        guard (sfName == symbolValT @name) $>
        case castTypeFilter @_ @_ @'ManualFilter sfFilter of
            Just (TypeManualFilter v) -> v
            _                         -> error "Failed to cast filter"
