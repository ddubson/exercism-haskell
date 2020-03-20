module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = collatzK n 0
        where
            collatzK n calls
                | n < 1 = Nothing
                | n == 1 = Just calls
                | even n = collatzK ((`div` 2) $ n) (calls+1)
                | otherwise = collatzK ((+1) . (* 3) $ n) (calls+1)