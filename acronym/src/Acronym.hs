module Acronym (abbreviate) where

import Data.Char

reduceIfAllCaps :: [Char] -> [Char]
reduceIfAllCaps arr = if all isUpper $ arr then [head arr] else arr

collectUpper :: [Char] -> [Char]
collectUpper [] = []
collectUpper (x:xs) = (toUpper x) : (foldr (\e acc -> if isUpper e then e:acc else acc) [] xs)

abbreviate :: String -> String
abbreviate xs =  foldr (\x acc -> (collectUpper x) ++ acc) []
    . map reduceIfAllCaps
    $ words [if elem x "-_" then ' ' else x | x <- xs]
