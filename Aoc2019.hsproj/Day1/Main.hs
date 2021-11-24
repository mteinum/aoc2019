-- AoC 2019, day 1

-- part1 logic
fuelRequired :: Int -> Int
fuelRequired mass = floor ((fromIntegral mass) / 3.0) - 2

-- test part1
cases = [(12,2),(14,2),(1969,654),(100756,33583)]

test :: [(Int,Int)] -> Bool -> Bool
test [] acc = acc
test ((a, b):xs) acc = test xs (fuelRequired a == b && acc)

-- fr :: Int -> Int -> Int

part1 :: IO Int
part1 = do
  content <- readFile("aoc-input-1.txt")
  let answer = sum .
               map fuelRequired .
               map read .
               lines $ content
  return (answer)
  

  

  