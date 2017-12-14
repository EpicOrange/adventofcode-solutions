# Problem 14

## Question 1

> The disk in question consists of a 128x128 grid; each square of the grid is either free or used. On this disk, the state of the grid is tracked by the bits in a sequence of knot hashes.
> 
> A total of 128 knot hashes are calculated, each corresponding to a single row in the grid; each hash contains 128 bits which correspond to individual grid squares. Each bit of a hash indicates whether that square is free (0) or used (1).
> 
> The hash inputs are a key string (your puzzle input), a dash, and a number from 0 to 127 corresponding to the row. For example, if your key string were flqrgnkx, then the first row would be given by the bits of the knot hash of flqrgnkx-0, the second row from the bits of the knot hash of flqrgnkx-1, and so on until the last row, flqrgnkx-127.
> 
> The output of a knot hash is traditionally represented by 32 hexadecimal digits; each of these digits correspond to 4 bits, for a total of 4 * 32 = 128 bits. To convert to bits, turn each hexadecimal digit to its equivalent binary value, high-bit first: 0 becomes 0000, 1 becomes 0001, e becomes 1110, f becomes 1111, and so on; a hash that begins with a0c2017... in hexadecimal would begin with 10100000110000100000000101110000... in binary.
> 
> Continuing this process, the first 8 rows and columns for key flqrgnkx appear as follows, using # to denote used squares, and . to denote free ones:
> 
>     ##.#.#..-->
>     .#.#.#.#   
>     ....#.#.   
>     #.#.##.#   
>     .##.#...   
>     ##..#..#   
>     .#...#..   
>     ##.#.##.-->
>     |      |   
>     V      V   
> 
> In this example, 8108 squares are used across the entire 128x128 grid.
> 
> Given your actual key string, how many squares are used?

We're reusing the knot hash from problem 10! Let's copy and paste that over (renaming `solve10'` to `knotHash` and removing `toHexString` since we don't need it for this problem):

    {-# LANGUAGE RecordWildCards #-}

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

It's asking for the total number of set bits after knot hashing `input-0` to `input-127`.

First, our input is a simple `String`. We can generate these input strings from the input by using `repeat` to create an infinite list of our input, and then zipping with `[0..127]`.

    makeInputs :: String -> [String]
    makeInputs = zipWith (\i s -> s ++ '-':(show i)) [0..127] . repeat

Then it's trivial to `map knotHash` to get a list of `[Word8]` results. We'll need a way to count bits for every `[Word8]`, and that's with `popCount` from `Data.Bits`:

    import Data.Bits (popCount)

    sumBits :: [Word8] -> Int
    sumBits = sum . map popCount

Now we apply `map (sumBits . knotHash)` to our generated inputs, and `sum` the result:

    solve14 :: String -> Int
    solve14 = sum . map (sumBits . knotHash) . makeInputs

## Question 2

> Now, all the defragmenter needs to know is the number of regions. A region is a group of used squares that are all adjacent, not including diagonals. Every used square is in exactly one region: lone used squares form their own isolated regions, while several adjacent squares all count as a single region.
> 
> In the example above, the following nine regions are visible, each marked with a distinct digit:
> 
>     11.2.3..-->
>     .1.2.3.4   
>     ....5.6.   
>     7.8.55.9   
>     .88.5...   
>     88..5..8   
>     .8...8..   
>     88.8.88.-->
>     |      |   
>     V      V   
> 
> Of particular interest is the region marked 8; while it does not appear contiguous in this small view, all of the squares marked 8 are connected when considering the whole 128x128 grid. In total, in this example, 1242 regions are present.
> 
> How many regions are present given your key string?

*todo: this*

Now we're doing a connected component problem. First, how do we represent our 128x128 bit grid? I chose to use a `Vector` of `Word128` (from the `largeword` package).

    import Data.Vector (Vector, (!), (!?), (//))
    import qualified Data.Vector as V
    import Data.LargeWord (Word128)

(Note that I used boxed `Vector` since `Word128` is boxed.)

This requires the output of `knotHash` (i.e. `[Word8]`) to be converted to `Word128`. Let's do that first.

`map fromIntegral` converts `[Word8]` into `[Word128]`. `zipWith (flip shiftL) [0,8..] . reverse` shifts the last `Word128` by 0 bits, the next one by 8, etc. Then our result comes from just combining these bytes with `.|.` (bitwise or).

    import Data.Bits (shiftL, (.|.))

    toWord128 :: [Word8] -> Word128
    toWord128 = foldr1 (.|.) . zipWith (flip shiftL) [0,8..] . reverse . map fromIntegral

`toWord128` lets us convert our knot hashes to the `Word128` representation. Then `V.fromList` makes that a vector, and now we have our `Vector Word128`!

    solve14' :: String -> Int
    solve14' = undefined . V.fromList . map (toWord128 . knotHash) . makeInputs

Now we need a function `Vector Word128 -> Int` that actually solves our connected component problem.

To get the number of groups, we can check every position for a `1` and clear its group. Then the number of groups is the number of times we need to call `clearGroup`.

In Haskell we can do this with a `length . filter clearGroup` on all possible positions (where clearGroup returns `True` if it finds a `1`. However, we have to update the grid state every time we call the filter function!. In other words, we should do this in the `State` monad:

    import Control.Monad.State

    countGroups :: Vector Word128 -> Int
    countGroups = evalState (length <$> filterM clearGroup everyPos)
      where everyPos = [(x, y) | x <- [0..127], y <- [0..127]]

By our definition we write `clearGroup` to `return True` if it cleared a group and `return False` otherwise.

    clearGroup :: (Int, Int) -> State (Vector Word128) Bool
    clearGroup (x,y) = do
      grid <- get
      if grid `isOneAt` (x,y)
        then do
             modify (`clearAt` (x,y))
             mapM_ clearGroup $ [(x-1,y), (x,y-1), (x+1,y), (x,y+1)]
             return True
        else return False

This relies on `isOneAt` (test if bit at `(x,y)` is `1`) and `clearAt` (clear bit at `(x,y)`):

    import Data.Bits (testBit, clearBit)

    isOneAt :: Vector Word128 -> (Int, Int) -> Bool
    grid `isOneAt` (x,y) = fmap (`testBit` x) (grid !? y) == Just True

    clearAt :: Vector Word128 -> (Int, Int) -> Vector Word128
    grid `clearAt` (x,y) = grid // [(y, (grid ! y) `clearBit` x)]

We can count groups now, so we're done!

    solve14' :: String -> Int
    solve14' = countGroups . V.fromList . map (toWord128 . knotHash) . makeInputs
