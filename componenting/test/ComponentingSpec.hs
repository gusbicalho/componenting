{-# LANGUAGE BlockArguments #-}
module ComponentingSpec where

import Test.Hspec
import Componenting

spec :: Spec
spec = do
  describe "test" do
    it "works" $
      (1 :: Int) `shouldBe` 1
