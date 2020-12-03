module Day2 (part1) where

import Util

data Policy = Policy
  { character :: Char,
    minOccurences :: Int,
    maxOccurences :: Int
  }
  deriving (Show)

newtype Password = Password String deriving (Show)

part1 :: IO ()
part1 = do
  content <- readFile "data/day2/part1.txt"
  print $ count (uncurry isValid . parse) $ lines content

parse :: String -> (Policy, Password)
parse s = (policy, Password password)
  where
    [policyRange, policyStr, password] = split ' ' s
    [policyMin, policyMax] = map read $ split '-' policyRange
    policy =
      Policy
        { character = head policyStr,
          minOccurences = policyMin,
          maxOccurences = policyMax
        }

isValid :: Policy -> Password -> Bool
isValid policy (Password p) = valid
  where
    charCount = count (== character policy) p
    valid = charCount >= minOccurences policy && charCount <= maxOccurences policy
