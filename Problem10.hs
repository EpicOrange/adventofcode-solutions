{-# LANGUAGE RecordWildCards #-}

module Problem10 where

import Data.Bits (xor)
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

knotHash :: [Word8] -> [Word8]
knotHash = unrotate . foldl' step (MyState [0..255] 0 0)
  where unrotate MyState{..} = rotate beginning string

solve10 :: String -> Int
solve10 = product . map fromIntegral . take 2
        . knotHash . map read . splitOn ","

convertInput :: String -> [Word8]
convertInput = (++ [17, 31, 73, 47, 23]) . map (fromIntegral . ord)

processInput :: [Word8] -> [Word8]
processInput = knotHash . concat . replicate 64

xorResult :: [Word8] -> [Word8]
xorResult = map (foldr1 xor) . chunksOf 16

toHexString :: [Word8] -> String
toHexString = foldr showHex "" 

solve10' = toHexString . xorResult . processInput . convertInput

input10 :: String
input10 = "206,63,255,131,65,80,238,157,254,24,133,2,16,0,1,3"
