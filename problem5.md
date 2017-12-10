# Problem 5

## Question 1

> The message includes a list of the offsets for each jump. Jumps are relative: -1 moves to the previous instruction, and 2 skips the next one. Start at the first instruction in the list. The goal is to follow the jumps until one leads outside the list.
> 
> In addition, these instructions are a little strange; after each jump, the offset of that instruction increases by 1. So, if you come across an offset of 3, you would move three instructions forward, but change it to a 4 for the next time it is encountered.
> 
> For example, consider the following list of jump offsets:
> 
> - 0
> - 3
> - 0
> - 1
> - -3
> 
> Positive jumps ("forward") move downward; negative jumps move upward. For legibility in this example, these offset values will be written all on one line, with the current instruction marked in parentheses. The following steps would be taken before an exit is found:
> 
> - (0) 3  0  1  -3  - before we have taken any steps.
> - (1) 3  0  1  -3  - jump with offset 0 (that is, don't jump at all). Fortunately, the instruction is then incremented to 1.
> -  2 (3) 0  1  -3  - step forward because of the instruction we just modified. The first instruction is incremented again, now to 2.
> -  2  4  0  1 (-3) - jump all the way to the end; leave a 4 behind.
> -  2 (4) 0  1  -2  - go back to where we just were; increment -3 to -2.
> -  2  5  0  1  -2  - jump 4 steps forward, escaping the maze.
> In this example, the exit is reached in 5 steps.
> How many steps does it take to reach the exit?

This problem is just asking for an `Int`-index-based *mutable* list. That's a `Vector`!

    import Data.Vector.Unboxed (Vector, (!), (!?))
    import qualified Data.Vector.Unboxed as V
    import qualified Data.Vector.Unboxed.Mutable as M

Here's my input type (obtained with `V.fromList . map read . lines` on the input string):

    input5 :: Vector Int

We're going to want to pair that with an index `i` starting at 0. Then every step we can get the value v at index i, and return a modified vector along with a modified i = i+v.

This behavior defines `step :: (Int, Vector Int) -> (Int, Vector Int)`; our code is written for us.

    step :: (Int, Vector Int) -> (Int, Vector Int)
    step (i, vec) = let v = vec ! i in
      (i+v, V.modify (\vec' -> M.write vec' i (v+1)) vec)

The problem states that it is possible that `i` is out of bounds, and when that happens, we want to stop the program. So let's perform this computation in the `Maybe` monad, indicating that when we get a `Nothing` value, we want to output a `Nothing` value back:

    step :: (Int, Vector Int) -> Maybe (Int, Vector Int)
    step (i, vec) = do
      v <- vec !? i
      return (i+v, V.modify (\vec' -> M.write vec' i (v+1)) vec)

We need to return a number of steps before we land outside the list. In Haskell, we'll need to count the number of times we call `step` before we get a `Nothing` value. We can do this with `length . unfoldr`. `unfoldr` can create a list of repeated applications using `step`, and `length` simply counts the number of those applications. Since we don't really care about the contents of the list we can fill it with `()`:

    import Data.List (unfoldr)

    solve5 i = length . unfoldr (fmap (\a -> ((), a)) . step) $ (0, i)

(Note how we turn the `Maybe (Int, Vector Int)` (returned by `step`) into a `Maybe ((), (Int, Vector Int)))`. This is because `unfoldr` wants `Maybe (listItem, nextInput)`. In this case we don't care about the generated list's contents so we give the unit value `()`.)

## Question 2

> Now, the jumps are even stranger: after each jump, if the offset was three or more, instead decrease it by 1. Otherwise, increase it by 1 as before.
> 
> Using this rule with the above example, the process now takes 10 steps, and the offset values after finding the exit are left as 2 3 2 3 -1.
> 
> How many steps does it now take to reach the exit?

Instead of adding one `(+1)`, we need to perform the following operation: `\v -> if v >= 3 then v - 1 else v + 1`

We easily shoehorn this into `step'`, expending only a few seconds:

    step' :: (Int, Vector Int) -> Maybe (Int, Vector Int)
    step' (i, vec) = do
      v <- vec !? i
      let v' = if v >= 3 then v - 1 else v + 1
      return (i+v, V.modify (\vec' -> M.write vec' i (v+1)) vec)

    solve5' i = length . unfoldr (fmap (\a -> ((), a)) . step') $ (0, i)

(Even better, instead of defining `step'`, we could give `step` a parameter `f :: Int -> Int` specifying the update function. This is left as an exercise to the reader.)