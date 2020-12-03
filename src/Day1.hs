module Day1
    (part1, part2) where

part1 :: IO ()
part1 = do
    content <- readFile "data/day1/input.txt"
    print $ map (uncurry (*)) $ filter pairIs2020 $ pairs $ map read $ lines content

pairs :: [Int] -> [(Int, Int)]
pairs l = [(x, y) | x <- l, y <- l]

pairIs2020 :: (Int, Int) -> Bool
pairIs2020 (a, b) = a + b == 2020


part2 :: IO ()
part2 = do
    content <- readFile "data/day1/input.txt"
    print $ map (\(a, b, c) -> a * b * c) $ filter tripleIs2020 $ triples $ map read $ lines content

triples :: [Int] -> [(Int, Int, Int)]
triples l = [(x, y, z) | x <- l, y <- l, z <- l]

tripleIs2020 :: (Int, Int, Int) -> Bool
tripleIs2020 (a, b, c) = a + b + c == 2020
