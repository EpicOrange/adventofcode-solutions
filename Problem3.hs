{-# LANGUAGE BangPatterns #-}

module Problem3 where

import Data.List (find, transpose)
import Data.Maybe (fromJust)

rotate = transpose . reverse

input3 :: Int
input3 = 325489

solve3 :: Int -> Int
solve3 i = 
  let n = fromJust . find ((>i) . (^2)) $ iterate (+2) 1 -- n^2 = first odd square >i
      r = n `div` 2 -- radius of square
      f = abs . (r-) . (`mod` n) -- the function f(d) = |r - (d%2r)|
  -- solution is on the square with radius r
  -- distance is r + (distance from side midpoint to corner)
  in r + f (n^2 - i)

solve3' :: Int -> Int
solve3' i = f [[1], [1]]
  where f !mat@(a:b:xs)
          | head a > i          = head a -- we're done
          | length b > length a =
              -- calculate next element of the spiral
              --  val:[26] <-- a
              --[2   1 25] <-- b
              -- 4   1 22
              -- 5  10 11
              let val = head a + (sum . take 3 . drop (length a - 1) . reverse $ b) in
              f $ (val:a):b:xs
          | otherwise           =
              -- row a is filled so rotate. first element of new row = head a + head b
              f $ [head a + head b]:(rotate mat)

