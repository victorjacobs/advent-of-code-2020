module Day4 (part1) where

import qualified Data.Set as Set
import Util

type Passport = Set.Set String

part1 :: IO ()
part1 = do
  content <- readFile "data/day4/part1.txt"
  print $ count isValid $ foldl parse [Set.empty] $ lines content

parse :: [Passport] -> String -> [Passport]
parse properties "" = Set.empty : properties
parse (lastProperties : xs) str = newProperties : xs
  where
    newProperties = Set.union lastProperties $ parseLine str

parseLine :: String -> Passport
parseLine str = Set.fromList properties
  where
    kvs = split ' ' str
    properties = map (head . split ':') kvs

isValid :: Passport -> Bool
isValid props = valid
  where
    requiredProperties =
      Set.fromList
        [ "byr",
          "iyr",
          "eyr",
          "hgt",
          "hcl",
          "ecl",
          "pid"
        ]
    valid = Set.intersection props requiredProperties == requiredProperties
