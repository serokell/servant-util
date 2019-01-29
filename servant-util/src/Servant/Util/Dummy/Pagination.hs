module Servant.Util.Dummy.Pagination
    ( paginate
    ) where

import qualified Data.List as L
import Universum

import Servant.Util.Combinators.Pagination
import Servant.Util.Internal.Util

paginate :: PaginationSpec -> [a] -> [a]
paginate PaginationSpec{..} =
    maybe id (L.genericTake . unPositive) psLimit . L.genericDrop psOffset
