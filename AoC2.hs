type Pos = (Int,Int)
origin :: Pos
origin = (0,0)

type R3 = (Int, Int, Int)
originR3 :: R3
originR3 = (0,0,0)

endPos :: [(String, Int)] -> Pos -> Pos
endPos []     (a,b) = (a,b)
endPos (x:xs) (a,b) | fst x == "forward" = endPos xs (a + snd x,b)
                    | fst x == "down"    = endPos xs (a,b  + snd x)
                    | fst x == "up"      = endPos xs (a,b - snd x)

aimFn :: [(String, Int)] -> R3 -> R3
aimFn []     (a,b,c) = (a,b,c)
aimFn (x:xs) (a,b,c) | fst x == "forward" = aimFn xs (a + snd x,b + c*(snd x),c)
                     | fst x == "down"    = aimFn xs (a,b,c + snd x)
                     | fst x == "up"      = aimFn xs (a,b,c - snd x)

parser :: [String] -> [(String, Int)]
parser []  = []
parser (x:xs) = [(x,y)] ++ parser (tail xs)
  where y = read (head xs) :: Int

prod3 :: R3 -> Int
prod3 (a,b,c) = a*b

main :: IO()
main = do
  -- Test Data
  let moves = [("forward", 5), ("down", 5), ("forward", 8), ("up", 3), ("down", 8), ("forward", 2)]
  let ans = endPos moves origin
  let product = fst ans * snd ans
  putStrLn "Final position: "
  print ans
  putStrLn "Multiplication: "
  print product
  let ans3 = aimFn moves originR3
  let product3 = prod3 ans3
  putStrLn "Final position (2): "
  print ans3
  putStrLn "Multiplication (2): "
  print product3

  -- Puzzle Input
  aocInput <- readFile "AoC2.txt"
  let aocText = words aocInput
  let aocData = parser aocText
  let ans = endPos aocData origin
  let product = fst ans * snd ans
  putStrLn "\nPuzze data:"
  putStrLn "Final position: "
  print ans
  putStrLn "Multiplication: "
  print product
  let ans3 = aimFn aocData originR3
  let product3 = prod3 ans3
  putStrLn "Final position (2): "
  print ans3
  putStrLn "Multiplication (2): "
  print product3

