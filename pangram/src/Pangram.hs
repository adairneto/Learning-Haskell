module Pangram (isPangram) where
import Data.Char

alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'x', 'w', 'y', 'z']

alphaChecker :: String -> [Char] -> Bool
alphaChecker _    []     = True
alphaChecker text (x:xs) = if elem x text then alphaChecker text xs else False

isPangram :: String -> Bool
isPangram text = alphaChecker (map toLower text) alphabet 
