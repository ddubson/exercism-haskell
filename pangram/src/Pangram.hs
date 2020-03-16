module Pangram (isPangram) where

import Data.Char (isAlpha, isAscii, toLower)
import Data.List (intersect)

isPangram :: String -> Bool
isPangram text = (==26) . length . (['a'..'z'] `intersect`). map (toLower) . filter onlyAsciiLetters $ text
    where
        onlyAsciiLetters = (\x -> isAlpha x && isAscii x)