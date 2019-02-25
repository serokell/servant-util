module Test.Servant.Filtering.GeneralSpec where

import Universum

import qualified Data.Map as M
import Test.Hspec (Spec, it)
import Test.QuickCheck (property)

import Servant.Util.Combinators.Filtering.Base
import Servant.Util.Combinators.Filtering.Support

-- | Traverses given filter types and ensures that defined operation
-- names are consistent.
class FilterOpsMatch (filters :: [* -> *]) where
    filterOpsMatch :: Either Text ()

instance FilterOpsMatch '[] where
    filterOpsMatch = pass

instance (IsAutoFilter filter, FilterOpsMatch filters) =>
         FilterOpsMatch (filter ': filters) where
    filterOpsMatch = do
        let opsEng = M.keysSet $ autoFilterEnglishOpsNames @filter
        let opsP = M.keysSet $ autoFilterParsers @filter @() Proxy
        unless (opsEng == opsP) $
            Left $ "autoFilterParsers and autoFilterEnglishOpsNames \
                   \define different set of operations \
                   \(" <> show opsP <> " and " <> show opsEng <> ")"

        filterOpsMatch @filters

spec :: Spec
spec = do
    it "autoFilterParsers & autoFilterEnglishOpsNames return \
       \the same set of operations" $

        property $ either error id $ filterOpsMatch @AllFilterTypes
