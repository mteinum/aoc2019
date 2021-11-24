module Day1 where
-- AoC 2019, day 1
-- https://adventofcode.com/2019/day/1

-- part1 logic
fuelRequired :: Int -> Int
fuelRequired mass = floor ((fromIntegral mass) / 3.0) - 2

-- test part1
cases = [(12,2),(14,2),(1969,654),(100756,33583)]

pass :: (Int,Int) -> Bool
pass (a, b) = fuelRequired a == b

testPart1 = all (== True) (map pass cases)


-- 3384232
part1 :: IO Int
part1 = do
  content <- readFile("aoc-input-1.txt")
  let answer = sum .
               map fuelRequired .
               map read .
               lines $ content
  return (answer)
  
--
-- Part 2
--
-- 5073456
--

-- fuelRequired2 :: Int -> Int -> Int
fuelRequired2_ mass acc
  | fuel <= 0 = sum acc
  | otherwise = fuelRequired2_ fuel (fuel:acc)
  where fuel = fuelRequired mass

fuelRequired2 mass = fuelRequired2_ mass []
  
part2 :: IO Int
part2 = do
  content <- readFile("aoc-input-1.txt")
  let answer = sum .
               map fuelRequired2 .
               map read .
               lines $ content
  return (answer)
  