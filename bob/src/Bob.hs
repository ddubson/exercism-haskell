module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isAscii, isUpper)

isMaybeEmpty :: [Char] -> Maybe [Char]
isMaybeEmpty [] = Nothing
isMaybeEmpty x = Just x

isMaybeShouting :: Maybe [Char] -> Bool
isMaybeShouting Nothing = False
isMaybeShouting (Just x) = all (isUpper) $ x

responseFor :: String -> String
responseFor xs = resolve . filter (\x -> not . isSpace $ x) $ xs

resolve :: String -> String
resolve xs
        | null xs = "Fine. Be that way!"
        | isAskingQuestion xs && isShouting xs = "Calm down, I know what I'm doing!"
        | isAskingQuestion xs = "Sure."
        | isShouting xs = "Whoa, chill out!"
        | otherwise = "Whatever."
        where
            isAskingQuestion = (=='?') . last
            onlyAsciiLetters = (\x -> isAlpha x && isAscii x)
            isShouting = isMaybeShouting . isMaybeEmpty . filter (onlyAsciiLetters)
