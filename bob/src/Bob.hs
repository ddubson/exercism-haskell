module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isAscii, isUpper)
import Data.Maybe (isNothing)

isMaybeEmpty :: [Char] -> Maybe [Char]
isMaybeEmpty [] = Nothing
isMaybeEmpty x = Just x

isMaybeShouting :: Maybe [Char] -> Bool
isMaybeShouting Nothing = False
isMaybeShouting (Just x) = all (isUpper) $ x

clean :: String -> String
clean str = filter (\x -> not . isSpace $ x) $ str

responseFor :: String -> String
responseFor xs = resolve . clean $ xs

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
            isSayingThings = isMaybeEmpty . filter (onlyAsciiLetters)
            isShouting = isMaybeShouting . isSayingThings
