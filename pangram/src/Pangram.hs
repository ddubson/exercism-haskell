module Pangram (isPangram) where

import Data.Char (isAlpha, isAscii, toLower)
import Data.List (sort)

isPangram :: String -> Bool
isPangram text = (== ['a'..'z']) . unique . sort . map (toLower) . filter onlyAsciiLetters $ text
    where
        unique = foldr (\x acc -> if x `elem` acc then acc else x:acc) []
        onlyAsciiLetters = (\x -> isAlpha x && isAscii x)

