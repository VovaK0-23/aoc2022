module Main where

import System.IO
import System.Environment
import Data.Char
import Data.Function
import Text.Printf

main = do
  args <- getArgs
  case args of
    (filePath:_) -> do
      handle <- openFile filePath ReadMode
      contents <- hGetContents handle
      printf "Part 1: %d\n" $ part1 contents
      printf "Part 2: %d\n" $ part2 contents
      hClose handle
    _ -> error "Input file is not provided"


part1 :: String -> Int
part1 contents =
  sumList $ map (priority . intersect . split) $ words contents

part2 contents =
  sumList $ map (priority . intersect) $ words contents & groupBy 3

priority :: String -> Int
priority (char:_) | isLower char = ord char - ord 'a' + 1
                    | otherwise = ord char - ord 'A' + 27

intersect :: [String] -> String
intersect xs = case xs of
  (a:b:c:_) -> f (\x -> elem x b && elem x c) a
  (a:b:_) -> f (\x -> elem x b) a
  where f x a =  (filter x a) & take 1

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

split :: [a] -> [[a]]
split myList =
        [(take listLen myList), (drop listLen myList)]
        where listLen =  length(myList) `div` 2

groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n xs = (take n xs):(groupBy n (drop n xs))

