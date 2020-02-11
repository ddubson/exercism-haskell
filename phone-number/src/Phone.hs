module Phone (number) where

import Data.Char (isDigit)

cleanInput :: String -> String
cleanInput input = foldr (\x acc -> if isDigit x then x:acc else acc) [] input

isValidLen :: String -> Maybe String
isValidLen input
    | lengthIsEleven && atIdx 4 '1' || atIdx 4 '0' = Nothing
    | lengthIsEleven && atIdx 1 '0' || atIdx 1 '1' = Nothing
    | lengthIsEleven && atIdx 0 '1' = Just (tail input)
    | lengthIsTen && atIdx 0 '0' || atIdx 0 '1' = Nothing
    | lengthIsTen && atIdx 3 '0' || atIdx 3 '1' = Nothing
    | lengthIsTen = Just input
    | otherwise = Nothing
    where
        lengthIsEleven = (==11) . length $ input
        lengthIsTen = (==10) . length $ input
        atIdx idx ch = (== ch) . (!! idx) $ input

number :: String -> Maybe String
number xs = isValidLen . cleanInput $ xs
