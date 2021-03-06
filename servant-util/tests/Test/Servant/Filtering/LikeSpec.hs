module Test.Servant.Filtering.LikeSpec where

import Test.Hspec (Spec, describe, it)
import Universum

import Servant.Util.Combinators.Filtering.Filters.Like

spec :: Spec
spec = do
    describe "LikePattern verification on construction" $ do
        it "Normal" $
            isRight $ mkLikePattern "abac.*."
        it "Lone escape" $
            isLeft $ mkLikePattern "a\\bac.*."
        it ". escaped" $
            isRight $ mkLikePattern "abac\\.*."
        it "* escaped" $
            isRight $ mkLikePattern "abac.\\*."
        it "Escape escaped" $
            isRight $ mkLikePattern "ab\\\\ac.*."
        it "Terminating escape" $
            isLeft $ mkLikePattern "abac.*.\\"
