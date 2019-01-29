{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Implements pagination in terms of `beam-postgres`.
module Servant.Util.Beam.Postgres.Pagination
    ( paginate_
    ) where

import Universum

import Database.Beam.Query (Q, limit_, offset_)
import Database.Beam.Query.Internal (QNested)

import Servant.Util.Combinators.Pagination
import Servant.Util.Internal.Util

-- | Truncate response according to the given pagination specification.
paginate_
    :: _
    => PaginationSpec
    -> Q select db (QNested (QNested s)) a
    -> Q select db s _
paginate_ PaginationSpec{..} =
    limit_ (maybe maxLimit (fromIntegral . unPositive) psLimit) .
    offset_ (fromIntegral psOffset)
  where
    -- We cannot just omit 'limit_' (types won't match),
    -- negative value does not work as well.
    -- So applying max value which can be passed to limit.
    maxLimit = fromIntegral (maxBound @Int64)
