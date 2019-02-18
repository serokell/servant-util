module Tests.Servant.Filtering.LikeSpec where

import Universum

import Servant.Util.Beam.Postgres.Filtering

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck ((===))

spec :: Spec
spec = do
    describe "Plain regex to SQL like syntax is converted fine" $ do
        it "No spec characters" $
            likeToSqlPattern "abc" === "abc"
        it "Simple replacement" $
            likeToSqlPattern "a.b*c" === "a_b%c"
        it "Escaping in source" $
            likeToSqlPattern "a\\.b\\*c" === "a.b*c"
        it "Escaping escaping in source" $
            likeToSqlPattern "a\\\\.b\\\\*c" === "a\\\\_b\\\\%c"
        it "Escaping" $
            likeToSqlPattern "a_b%c" === "a\\_b\\%c"
