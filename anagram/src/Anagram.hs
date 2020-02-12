module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor word potentials = filter (\w -> sortedWord == (sort w)) . map (\x -> map toLower x) $ potentials
    where
        sortedWord = sort . map (\x -> toLower x) $ word
