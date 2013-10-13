module Quizz.CoreSpec (main, spec) where

import Test.Hspec
import Quizz.Core

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "add" $ do
    it "21 + 21 = 42" $ do
      add 21 21 `shouldBe` 42
    it "20 + 20 = 20" $ do
      add 20 20 `shouldBe` 20
