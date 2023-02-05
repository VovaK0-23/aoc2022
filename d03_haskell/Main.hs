module Main where

import Data.Char (isLower, ord)
import Data.Function ((&))
import System.Environment (getArgs)
import System.IO
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- check if input file is provided or not
    (filePath : _) -> do
      -- open input file
      handle <- openFile filePath ReadMode
      -- read contents of the file
      contents <- hGetContents handle
      -- print the result
      printf "Part 1: %d\n" $ part1 contents
      printf "Part 2: %d\n" $ part2 contents
      -- close the input file
      hClose handle
    -- if there is no argument passed, raise an error message
    _ -> error "Input file is not provided"

-- The part1 function takes the contents of the input file as a string,
-- splits the contents into words,
-- finds the intersections of words,
-- calculates the priority of the first character of the intersected word,
-- and returns the sum of all priorities.
part1 :: String -> Int
part1 contents =
  sum $ map (priority . intersect . split) $ words contents

-- The part2 function is similar to part1,
-- but it groups the words into sublists of three
-- and finds the intersections of words from these sublists.
part2 :: String -> Int
part2 contents =
  sum $ map (priority . intersect) . groupBy 3 $ words contents

priority :: Char -> Int
priority char
  | isLower char = ord char - ord 'a' + 1
  | otherwise = ord char - ord 'A' + 27

-- The intersect function takes a list of strings
-- and returns the first character of the string that is present in all other strings.
intersect :: [String] -> Char
intersect xs = case xs of
  (a : b : c : _) -> head (f (\x -> elem x b && elem x c) a)
  (a : b : _) -> head (f (`elem` b) a)
  where
    f x a = filter x a & take 1

split :: [a] -> [[a]]
split myList =
  [take listLen myList, drop listLen myList]
  where
    listLen = length myList `div` 2

-- Groups elements of a list into sublists of a specified length
groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n xs = take n xs : groupBy n (drop n xs)
