# Problem 8

## Question 1

> Each instruction consists of several parts: the register to modify, whether to increase or decrease that register's value, the amount by which to increase or decrease it, and a condition. If the condition fails, skip the instruction without modifying the register. The registers all start at 0. The instructions look like this:
> 
> - b inc 5 if a > 1
> - a inc 1 if b < 5
> - c dec -10 if a >= 1
> - c inc -20 if c == 10
> 
> These instructions would be processed as follows:
> 
> Because a starts at 0, it is not greater than 1, and so b is not modified.
> a is increased by 1 (to 1) because b is less than 5 (it is 0).
> c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
> c is increased by -20 (to -10) because c is equal to 10.
> After this process, the largest value in any register is 1.
> 
> You might also encounter <= (less than or equal to) or != (not equal to). However, the CPU doesn't have the bandwidth to tell you what all the registers are named, and leaves that to you to determine.
> 
> What is the largest value in any register after completing the instructions in your puzzle input?

This problem asks us to consume each line (top-to-bottom) and update some state along the way. That's just calling for a `foldl'` of some kind: we just need to write the fold function. Also, we'll want to use a `Map` state since we're storing values with string identifiers.

    import Data.List (foldl')
    import Data.Map.Strict (Map, (!))
    import qualified Data.Map.Strict as Map

Now we'd like to have a function for `foldl'` that will `execute` a line. Since every line has the same number of tokens we can just use `words` and pattern matching to capture every token:

    execute :: Map String Int -> [String] -> Map String Int
    execute m [var, incOrDec, valStr, _,
               checkVar, op, checkValStr] = 
      let val = read valStr
          checkVal = read checkValStr
      in undefined

As an aside, let's bring in `-XViewPatterns` to eliminate `valStr` and `checkValStr` (we also need `-XScopedTypeVariables` for the inline type annotations):

    {-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

    execute :: Map String Int -> [String] -> Map String Int
    execute m [var, incOrDec, read -> val :: Int, _,
               checkVar, op, read -> checkVal :: Int] =
      undefined

After this tokenizing, executing is simple. We want to check the condition and update a value. First let's define `check`:

    check l "<" r = l < r
    check l ">" r = l > r
    check l "<=" r = l <= r
    check l ">=" r = l >= r
    check l "==" r = l == r
    check l "!=" r = l /= r

Now `execute` follows from the definition: check the condition and update a value.

    execute :: Map String Int -> [String] -> Map String Int
    execute m [var, incOrDec, read -> val :: Int, _,
               checkVar, op, read -> checkVal :: Int]
      | check (Map.findWithDefault 0 checkVar m) op checkVal
          = Map.alter (update incOrDec val) var m
      | otherwise = m

We could use `Map.adjust f` which maps a function `f :: v -> v` on the value associated with a given key in a `Map`. However that key may not exist so instead we use `Map.alter f` instead which takes `f :: Maybe v -> Maybe v`, providing the value `Nothing` for undefined keys.

The only updates we do are increments and decrements, depending on the value of `incOrDec`. So the `update` function we pass to `Map.alter` is:

    update :: String -> Int -> (Maybe Int -> Maybe Int)
    update "inc" val Nothing  = Just $ val
    update "inc" val (Just v) = Just $ v + val
    update "dec" val Nothing  = Just $ -val
    update "dec" val (Just v) = Just $ v - val

I partially apply `incOrDec` and `val` and then give the resulting `(Maybe Int -> Maybe Int)` to `Map.alter`.

Now we just need to write the fold. We `foldl'` on the input with `execute`, starting with an empty map. Then the question asks us to provide the maximum value in the map, which is just `maximum . Map.elems`.

    solve8 :: [[String]] -> Int
    solve8 = maximum . Map.elems . foldl' execute Map.empty

By the way, my input type is `[[String]]`: a list of tokenized lines.

    input8 :: [[String]]
    input8 = map words . lines $ <the input string>

## Question 2

> To be safe, the CPU also needs to know the highest value held in any register during this process so that it can decide how much memory to allocate to these operations. For example, in the above instructions, the highest value ever held was 10 (in register c after the third instruction was evaluated).

This is fine; we just need to add an `Int` `highest` to our accumulator state to track the highest value and edit `execute` accordingly. Then `solve8'` can discard the map, returning this.


    execute :: (Int, Map String Int) -> [String] -> (Int, Map String Int)
    execute (highest, m) [var, incOrDec, read -> val :: Int, _,
                          checkVar, op, read -> checkVal :: Int]
      | check (Map.findWithDefault 0 checkVar m) op checkVal
          = let m' = Map.alter (update incOrDec val) var m
            in (max highest $ m' ! var, m')
      | otherwise = (highest, m)

    solve8' :: [[String]] -> Int
    solve8' = fst . foldl' execute Map.empty

We can make this state compatible with our first problem with `snd`:

    solve8 :: [[String]] -> Int
    solve8 = maximum . Map.elems . snd . foldl' execute (0, Map.empty)