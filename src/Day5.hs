module Day5 (part1, parseLine, idForRange) where

type Range = (Int, Int)

type CoordinateRange = (Range, Range)

part1 :: IO ()
part1 = do
  content <- readFile "data/day5/part1.txt"
  print $ maximum $ map (idForRange . parseLine ((0, 7), (0, 127))) $ lines content

parseLine :: CoordinateRange -> String -> CoordinateRange
parseLine coord "" = coord
parseLine (x, (yLow, yHigh)) ('F' : xs) = parseLine (x, (yLow, yHigh - ceiling (fromIntegral (yHigh - yLow) / 2.0))) xs
parseLine (x, (yLow, yHigh)) ('B' : xs) = parseLine (x, (yLow + ceiling (fromIntegral (yHigh - yLow) / 2.0), yHigh)) xs
parseLine ((xLow, xHigh), y) ('L' : xs) = parseLine ((xLow, xHigh - ceiling (fromIntegral (xHigh - xLow) / 2.0)), y) xs
parseLine ((xLow, xHigh), y) ('R' : xs) = parseLine ((xLow + ceiling (fromIntegral (xHigh - xLow) / 2.0), xHigh), y) xs

idForRange :: CoordinateRange -> Int
idForRange ((_, xHigh), (yLow, _)) = yLow * 8 + xHigh
