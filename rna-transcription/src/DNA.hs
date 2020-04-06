module DNA (toRNA) where

import Data.Either (fromRight)

toRNA :: String -> Either Char String
toRNA "" = Right ""
toRNA "C" = Right "G"
toRNA "G" = Right "C"
toRNA "T" = Right "A"
toRNA "A" = Right "U"
toRNA (x:xs) = (toRNA x:[]) ++ (toRNA $ xs)