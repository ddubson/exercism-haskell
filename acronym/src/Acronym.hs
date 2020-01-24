module Acronym (abbreviate) where

import Data.Char

allCaps :: [Char] -> Bool
allCaps = not . all (isUpper)

collectUpper :: [Char] -> [Char]
collectUpper (x:xs) = (toUpper x) : (foldr (\e acc -> if isUpper e then e:acc else acc) [] xs)

abbreviate :: String -> String
abbreviate xs =  foldr (\x acc -> (collectUpper x) ++ acc) [] . filter allCaps $ words xs
