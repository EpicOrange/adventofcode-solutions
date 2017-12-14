{-# LANGUAGE RecordWildCards #-}

module Problem14 where

import Data.Bits (xor, popCount)
import Data.Char (ord)
import Data.List (foldl', foldr1)
import Data.List.Split (splitOn, chunksOf)
import Data.Word (Word8)
import Numeric (showHex)

data MyState = MyState { string :: [Word8]
                       , skip :: Word8
                       , beginning :: Word8} deriving Show

rotate :: Word8 -> [Word8] -> [Word8]
rotate n = (\(f, b) -> b ++ f) . splitAt (fromIntegral n)

twist :: Word8 -> [Word8] -> [Word8]
twist n = (\(f, b) -> reverse f ++ b) . splitAt (fromIntegral n)

step :: MyState -> Word8 -> MyState
step MyState{..} l = MyState (rotate n . twist l $ string) (skip + 1) (beginning - n)
  where n = (l + skip)

knotHashStep :: [Word8] -> [Word8]
knotHashStep = unrotate . foldl' step (MyState [0..255] 0 0)
  where unrotate MyState{..} = rotate beginning string

convertInput :: String -> [Word8]
convertInput = (++ [17, 31, 73, 47, 23]) . map (fromIntegral . ord)

processInput :: [Word8] -> [Word8]
processInput = knotHashStep . concat . replicate 64

xorResult :: [Word8] -> [Word8]
xorResult = map (foldr1 xor) . chunksOf 16

knotHash = xorResult . processInput . convertInput

makeInputs :: String -> [String]
makeInputs = zipWith (\i s -> s ++ '-':(show i)) [0..127] . repeat

sumBits :: [Word8] -> Int
sumBits = sum . map popCount

solve14 :: String -> Int
solve14 = sum . map (sumBits . knotHash) . makeInputs

input14 :: String
input14 = "jxqlasbh"
