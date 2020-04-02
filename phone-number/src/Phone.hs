module Phone (number) where

import Data.Char (isDigit, isAlpha)

type MaybeString = String -> Maybe String

exchangeCodeCheck :: MaybeString
exchangeCodeCheck input | input !! 3 == '1' || input !! 3 == '0' = Nothing
                        | otherwise = Just input

areaCodeCheck :: MaybeString
areaCodeCheck (x:xs) | x == '1' || x == '0' = Nothing
                     | otherwise = Just (x:xs)

illegalCharCheck :: MaybeString
illegalCharCheck input | any (\x -> isAlpha x || x == '!') input = Nothing
                       | otherwise = Just input

isValidLen :: MaybeString
isValidLen (x:xs) | numberLength == 11 = Just (x:xs) >>= countryCodeCheck
                                                     >>= areaCodeCheck
                                                     >>= exchangeCodeCheck
                  | numberLength == 9 || numberLength > 11 = Nothing
                  | otherwise = Just (x:xs)
                  where
                    countryCodeCheck (y:ys) | y /= '1' = Nothing
                                            | otherwise = Just ys
                    numberLength = length (x:xs)

number :: MaybeString
number xs = Just xs >>= illegalCharCheck
                    >>= filterByDigit
                    >>= isValidLen
                    >>= areaCodeCheck
                    >>= exchangeCodeCheck
            where
              filterByDigit n = Just (filter isDigit n)
