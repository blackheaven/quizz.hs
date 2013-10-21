module Quizz.CliSpec (main, spec) where

import Test.Hspec
import Quizz.Core
import Quizz.Cli

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Print question" $ do
        it "with one proposition" $ do
            askQuestion (Question "A" [Proposition "B" True]) `shouldBe` unlines ["A", "1. B"]
        it "with two propositions" $ do
            askQuestion (Question "A" [ Proposition "B" True,
                                        Proposition "C" False]) `shouldBe`
                                        unlines ["A", "1. B", "2. C"]
        it "with for propositions" $ do
            askQuestion (Question "A" [ Proposition "B" True,
                                        Proposition "C" False,
                                        Proposition "B" True,
                                        Proposition "C" False]) `shouldBe`
                                        unlines ["A", "1. B", "2. C", "3. B", "4. C"]
