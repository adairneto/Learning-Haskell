isGreater :: [Int] -> Int -> Int
isGreater (x:xs) n | xs == []    = n
                   | x < head xs = isGreater xs n+1
                   | otherwise   = isGreater xs n

isGreater3 :: [Int] -> Int -> Int
isGreater3 (x:xs) n | length xs < 3                             = n
                    | (sum . take 3) (x:xs) < (sum . take 3) xs = isGreater3 xs n+1
                    | otherwise                                 = isGreater3 xs n

convertData :: String -> [Int]
convertData text = map read $ words text :: [Int]

main = do
  let testData = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
  let testAns = isGreater testData 0
  let testAns3 = isGreater3 testData 0
  putStrLn "Test Data\n"
  print testAns
  print testAns3

  aocText <- readFile "AoC1.txt"
  let aocData = convertData aocText
  let aocAns = isGreater aocData 0
  let aocAns3 = isGreater3 aocData 0
  putStrLn "\nAoC Data\n"
  print aocAns
  print aocAns3
