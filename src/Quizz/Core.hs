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

newtype Answers = Answers { answers :: [Integer] } deriving (Show)

newtype Corrections = Corrections { corrections :: [Correction] } deriving (Show, Eq)

data Correction = Correct | Wrong | Missed | NotCheckedNotAnswer deriving (Show, Eq)

correct :: Question -> Answers -> Corrections
correct (Question _ propositions) (Answers answersIndices) =
    Corrections $ zipWith check propositions answersChecked
    where answersChecked = switchTrue (map (subtract 1) answersIndices) $ replicate (length propositions) False

check :: Proposition -> Bool -> Correction
check (Proposition _ True) True = Correct
check (Proposition _ True) False = Missed
check (Proposition _ False) True = Wrong
check (Proposition _ False) False = NotCheckedNotAnswer

switchTrue :: [Integer] -> [Bool] -> [Bool]
switchTrue indices set = switchTrue' 0 indices set []
    where   switchTrue' _ [] s acc = (reverse acc) ++ s
            switchTrue' _ _ [] acc = reverse acc
            switchTrue' indice (i:is) (x:xs) acc
                | i == indice = switchTrue' (indice + 1) is xs (True:acc)
                | otherwise = switchTrue' (indice + 1) (i:is) xs (x:acc)
