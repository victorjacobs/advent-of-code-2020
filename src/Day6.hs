module Day6 (part1) where

import qualified Data.Set as Set

type Form = Set.Set Char

type FormGroup = [Form]

part1 :: IO ()
part1 = do
  content <- readFile "data/day6/part1.txt"
  print $ sum $ map (Set.size . foldl1 Set.union) $ foldl parse [] $ lines content

parse :: [FormGroup] -> String -> [FormGroup]
parse [] str = [[parseLine str]]
parse formGroups "" = [] : formGroups
parse (formGroup : xs) str = (parseLine str : formGroup) : xs

parseLine :: String -> Form
parseLine = Set.fromList
