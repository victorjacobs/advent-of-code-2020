module Day3 (part1, parseLine) where

import qualified Data.Map.Strict as Map
import Util

data Coordinate = Coordinate
  { x :: Int,
    y :: Int
  }
  deriving (Show, Ord, Eq)

newtype Tree = Tree Bool deriving (Show)

type TreeMap = Map.Map Coordinate Tree

part1 :: IO ()
part1 = do
  content <- readFile "data/day3/part1.txt"
  print $ treesEncountered $ parse $ lines content

treesEncountered :: TreeMap -> Int
treesEncountered m = count (== True) $ [(`hasTree` m) (Coordinate x y) | (x, y) <- zip [0, 3 ..] [0 .. 400]]

hasTree :: Coordinate -> TreeMap -> Bool
hasTree c m
  | Nothing <- t = False
  | Just (Tree r) <- t = r
  where
    clampedCoord = Coordinate (x c `mod` 31) (y c)
    t = Map.lookup clampedCoord m

parse :: [String] -> TreeMap
parse strs = foldl Map.union Map.empty (zipWith parseLine strs [0 ..])

parseLine :: String -> Int -> TreeMap
parseLine str y = foldl Map.union Map.empty $ zipWith (parseCoordinate y) [0 ..] str

parseCoordinate :: Int -> Int -> Char -> TreeMap
parseCoordinate y x '.' = Map.singleton (Coordinate x y) (Tree False)
parseCoordinate y x '#' = Map.singleton (Coordinate x y) (Tree True)
