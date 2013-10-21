module Quizz.Cli (
        askQuestion
        )  where

import Quizz.Core

askQuestion :: Question -> String
askQuestion (Question title propositions) = unlines $ title : zipWith (\n t -> (show n) ++ ". " ++ content t) [1..] propositions
