module PrimeFactors (primeFactors) where

factors :: Integer -> [Integer]  
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Integer -> Bool
prime n = factors n == [1,n]

factorAux :: Integer -> Integer -> [Integer] -> [Integer]
factorAux 1 m [a] = [a]
factorAux n m []  | n == m                    = []
                  | n `mod` m == 0 && prime m = factorAux (n `div` m) 2 [m]
                  | otherwise                 = factorAux n (m+1) []
factorAux n m [a] | n `mod` m == 0 && prime m = factorAux (n `div` m) m [m] ++ [a]
                  | otherwise                 = factorAux n (m+1) [a]

primeFactors :: Integer -> [Integer]
primeFactors n = reverse (factorAux n 2 [])