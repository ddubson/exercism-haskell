module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA input = traverse (translate) input
                  where
                    translate 'C' = Right 'G'
                    translate 'G' = Right 'C'
                    translate 'T' = Right 'A'
                    translate 'A' = Right 'U'
                    translate otherChar = Left otherChar
