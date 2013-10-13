module Quizz.Core (
        Proposition(Proposition), isValid, content,
        Question(Question), title, propositions,
        Answers(Answers), answers,
        Corrections(Corrections), corrections,
        Correction(Correct, Wrong, Missed, NotCheckedNotAnswer), correct
        )  where

data Proposition = Proposition  { content :: String
                                , isValid :: Bool
                                } deriving (Show)

data Question = Question    { title :: String
                            , propositions :: [Proposition]
                            } deriving (Show)

newtype Answers = Answers { answers :: [Bool] } deriving (Show)

newtype Corrections = Corrections { corrections :: [Correction] } deriving (Show, Eq)

data Correction = Correct | Wrong | Missed | NotCheckedNotAnswer deriving (Show, Eq)

correct :: Question -> Answers -> Corrections
correct (Question _ propositions) (Answers answers) =
    Corrections $ zipWith check propositions answers

check :: Proposition -> Bool -> Correction
check (Proposition _ True) True = Correct
check (Proposition _ True) False = Missed
check (Proposition _ False) True = Wrong
check (Proposition _ False) False = NotCheckedNotAnswer
