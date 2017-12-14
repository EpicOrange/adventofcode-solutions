module Problem13 where

import Data.List.Split (splitOn)

severity :: (Int, Int) -> Int
severity (depth, range)
  | depth `mod` (2 * (range - 1)) == 0 = depth * range
  | otherwise                          = 0

solve13, solve13' :: [(Int, Int)] -> Int
solve13 = sum . map severity

successful :: Int -> [(Int, Int)] -> Bool
successful delay = all (\(d, r) -> severity (d+delay, r) == 0)

solve13' i = head $ filter (flip successful i) [0..]

input13 :: [(Int, Int)]
input13 = map ((\[l,r] -> (l,r)) . map read . splitOn ": ") $ [
    "0: 4",
    "1: 2",
    "2: 3",
    "4: 4",
    "6: 8",
    "8: 5",
    "10: 8",
    "12: 6",
    "14: 6",
    "16: 8",
    "18: 6",
    "20: 6",
    "22: 12",
    "24: 12",
    "26: 10",
    "28: 8",
    "30: 12",
    "32: 8",
    "34: 12",
    "36: 9",
    "38: 12",
    "40: 8",
    "42: 12",
    "44: 17",
    "46: 14",
    "48: 12",
    "50: 10",
    "52: 20",
    "54: 12",
    "56: 14",
    "58: 14",
    "60: 14",
    "62: 12",
    "64: 14",
    "66: 14",
    "68: 14",
    "70: 14",
    "72: 12",
    "74: 14",
    "76: 14",
    "80: 14",
    "84: 18",
    "88: 14"]
