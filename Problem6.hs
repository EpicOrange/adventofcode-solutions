module Problem6 where

import Data.List (unfoldr)
import Data.Vector.Unboxed (Vector, (!), (!?))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type MyState = (Vector Int, Map (Vector Int) Int, Int)

step :: Vector Int -> Vector Int
step vec =
  let i = V.maxIndex vec
      v = vec ! i
      vec' = V.modify (\vec' -> M.write vec' i 0) vec
      indices = map (`mod` (V.length vec)) [i+1 .. i+v]
  in V.accum (flip ($)) vec' [(j, (+1)) | j <- indices]

loop :: MyState -> MyState
loop = until (\(vec, map, i) -> vec `Map.member` map)
             (\(vec, map, i) -> (step vec, Map.insert vec i map, i+1))

solve6, solve6' :: Vector Int -> Int
solve6 i = n where (_, _, n) = loop (i, Map.empty, 0)
solve6' i = n - (map Map.! vec)
  where (vec, map, n) = loop (i, Map.empty, 0)

input6 :: Vector Int
input6 = V.fromList [5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6]
