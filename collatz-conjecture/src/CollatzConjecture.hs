module CollatzConjecture (collatz) where

collatzCounter :: Integer -> Integer -> Maybe Integer
collatzCounter steps n
    | n < 1 = Nothing
    | n == 1 = Just steps
    | even n = collatzCounter (steps + 1) (n `div` 2)
    | odd n = collatzCounter (steps + 1) (3 * n + 1)

collatz :: Integer -> Maybe Integer
collatz = collatzCounter 0
