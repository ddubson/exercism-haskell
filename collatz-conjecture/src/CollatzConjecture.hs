module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n | n < 1 = Nothing
          | otherwise = collatzK n 0
            where
                collatzK x calls
                    | x == 1 = Just calls
                    | otherwise = collatzK result (calls+1)
                    where
                      result = if even x then (x `quot` 2) else (x * 3 + 1)

