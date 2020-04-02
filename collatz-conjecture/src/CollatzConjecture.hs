module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = collatzK n 0
        where
            collatzK x calls
                | x < 1 = Nothing
                | x == 1 = Just calls
                | otherwise = collatzK result (calls+1)
                where
                  result = if even x then fst (x `quotRem` 2) else (x * 3 + 1)

