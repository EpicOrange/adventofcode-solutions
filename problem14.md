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