module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate xs =  join . map . splitOn " " $ xs
--foldr (\x acc -> if isUpper x then x:acc else acc) [] xs
