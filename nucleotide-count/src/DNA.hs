module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, insertWith, size)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

toNucleotide :: String -> Maybe Nucleotide
toNucleotide "A" = Just A
toNucleotide "C" = Just C
toNucleotide "G" = Just G
toNucleotide "T" = Just T
toNucleotide x =  Nothing

emptyMap :: Map Nucleotide Int
emptyMap = fromList [(A,0),(C,0),(G,0),(T,0)]

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = let
                        maybeNucleoString = map (toNucleotide) $ (:[]) <$> xs
                        builtMap = foldr insert emptyMap $ fromJust $ maybeNucleoString
                                    where
                                      insert x agg = insertWith (+) x 1 agg
                      in
                        case maybeNucleoString of
                          Nothing -> Left
                          otherwise -> Right builtMap
