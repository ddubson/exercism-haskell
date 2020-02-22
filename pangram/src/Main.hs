module Main (main) where

import Data.Char (isAlpha, toLower)
import Data.List (sort)

unique :: String -> String
unique text = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] text

main :: IO ()
main = do
    let s = "the quick brown fox jumps over the lazy dog"
    let al = ['a'..'z']
    print $ sort . unique . map (toLower) . filter (isAlpha) $ s