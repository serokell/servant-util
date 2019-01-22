{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Implements pagination in terms of `beam-postgres`.
module Servant.Util.Beam.Postgres.Pagination
    ( paginate_
    ) where

import Universum

import Database.Beam.Query (Q, limit_, offset_)
import Database.Beam.Query.Internal (QNested)

import Servant.Util.Pagination

-- | Truncate response according to the given pagination specification.
paginate_
    :: _
    => PaginationSpec
    -> Q select db (QNested (QNested s)) a
    -> Q select db s _
paginate_ PaginationSpec{..} =
     limit_ (fromIntegral psLimit) . offset_ (fromIntegral psOffset)
