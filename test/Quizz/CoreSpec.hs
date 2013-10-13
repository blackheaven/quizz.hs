module Quizz.CoreSpec (main, spec) where

import Test.Hspec
import Quizz.Core

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Proposition datatype" $ do
    describe "isValid getter" $ do
      it "A valid proposition" $ do
        isValid (Proposition "A" True) `shouldBe` True
      it "An invalid proposition" $ do
        isValid (Proposition "A" False) `shouldBe` False

    describe "content getter" $ do
      it "A simple content" $ do
        content (Proposition "A" True) `shouldBe` "A"

  describe "Question datatype" $ do
    describe "title getter" $ do
      it "A simple title" $ do
        title (Question "A" []) `shouldBe` "A"

    describe "propositions getter" $ do
      it "No proposition" $ do
        (length . propositions) (Question "A" []) `shouldBe` 0
      it "One proposition" $ do
        (length . propositions) (Question "A" [Proposition "A" True]) `shouldBe` 1
      it "Two propositions" $ do
        (length . propositions) (Question "A" [Proposition "A" True, Proposition "A" True]) `shouldBe` 2

  describe "Answers datatype" $ do
    describe "answers" $ do
      it "No answer" $ do
        (length . answers) (Answers []) `shouldBe` 0
      it "One answer" $ do
        (length . answers) (Answers [True]) `shouldBe` 1
      it "Four answers" $ do
        (length . answers) (Answers [True, False, False, True]) `shouldBe` 4

  describe "Corrections datatype" $ do
    describe "corrections" $ do
      it "No correction" $ do
        (length . corrections) (Corrections []) `shouldBe` 0
      it "One correction" $ do
        (length . corrections) (Corrections [Correct]) `shouldBe` 1
      it "Two corrections" $ do
        (length . corrections) (Corrections [Wrong, Missed]) `shouldBe` 2
    describe "correct" $ do
      it "One proposition, one good answer, correctly answered" $ do
        correct (Question "A" [Proposition "A" True]) (Answers [True])`shouldBe` (Corrections [Correct])
      it "One proposition, one good answer, not answered" $ do
        correct (Question "A" [Proposition "A" True]) (Answers [False])`shouldBe` (Corrections [Missed])
      it "One proposition, no good answer, badly answered" $ do
        correct (Question "A" [Proposition "A" False]) (Answers [True])`shouldBe` (Corrections [Wrong])
      it "One proposition, no good answer, correctly answered" $ do
        correct (Question "A" [Proposition "A" False]) (Answers [False])`shouldBe` (Corrections [NotCheckedNotAnswer])
      it "Four propositions, two good answers, one correctly answered, one badly answered, one forgot" $ do
        correct (Question "A" [Proposition "A" True, Proposition "A" True, Proposition "A" False, Proposition "A" False]) (Answers [True, False, True, False])`shouldBe` (Corrections [Correct, Missed, Wrong, NotCheckedNotAnswer])
