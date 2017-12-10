{-# LANGUAGE BangPatterns #-}

module Problem3 where

import Data.List (find, transpose)
import Data.Maybe (fromJust)

rotate = transpose . reverse

input3 :: Int
input3 = 325489

solve3, solve3' :: Int -> Int
solve3 i = r + d
  where n = head . dropWhile ((<=i) . (^2)) $ iterate (+2) 1
        r = n `div` 2
        x = (n^2 - i) `mod` n
        d = abs $ r - x

solve3' i = head . dropWhile (<=i) . head . until (\(a:_) -> last a >= i) update $ [[1]]
  where update :: [[Int]] -> [[Int]]
        update mat = (makeRow r):mat'
          where mat'@(r:_) = (reverse . transpose) mat
        makeRow :: [Int] -> [Int]
        makeRow xs = scanl1 (+) $ zipWith3 add3 (0:xs) xs (tail xs ++ [0])
          where add3 a b c = a+b+c
