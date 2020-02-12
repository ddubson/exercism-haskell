module Hamming (distance) where

calcDiff :: [(Char, Char)] -> Int
calcDiff p = foldl (\acc x -> if (fst x) /= (snd x) then (acc + 1) else acc) 0 p

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs /= length ys = Nothing
    | otherwise = Just . calcDiff $ zip xs ys
