module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = collatzK n 0
        where
            collatzK x calls
                | x < 1 = Nothing
                | x == 1 = Just calls
                | otherwise = collatzK (
                    let (t, r) = x `quotRem` 2
                    in
                      if r == 0 then t else (x * 3 + 1)) (calls + 1)
