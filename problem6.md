# Problem 6

## Question 1

> In this area, there are sixteen memory banks; each memory bank can hold any number of blocks. The goal of the reallocation routine is to balance the blocks between the memory banks.
> 
> The reallocation routine operates in cycles. In each cycle, it finds the memory bank with the most blocks (ties won by the lowest-numbered memory bank) and redistributes those blocks among the banks. To do this, it removes all of the blocks from the selected bank, then moves to the next (by index) memory bank and inserts one of the blocks. It continues doing this until it runs out of blocks; if it reaches the last memory bank, it wraps around to the first one.
> 
> The debugger would like to know how many redistributions can be done before a blocks-in-banks configuration is produced that has been seen before.
> 
> For example, imagine a scenario with only four memory banks:
> 
> - The banks start with 0, 2, 7, and 0 blocks. The third bank has the most blocks, so it is chosen for redistribution.
> - Starting with the next bank (the fourth bank) and then continuing to the first bank, the second bank, and so on, the 7 blocks are spread out over the memory banks. The fourth, first, and second banks get two blocks each, and the third bank gets one back. The final result looks like this: 2 4 1 2.
> - Next, the second bank is chosen because it contains the most blocks (four). Because there are four memory banks, each gets one block. The result is: 3 1 2 3.
> - Now, there is a tie between the first and fourth memory banks, both of which have three blocks. The first bank wins the tie, and its three blocks are distributed evenly over the other three banks, leaving it with none: 0 2 3 4.
> - The fourth bank is chosen, and its four blocks are distributed such that each of the four banks receives one: 1 3 4 1.
> - The third bank is chosen, and the same thing happens: 2 4 1 2.
> - At this point, we've reached a state we've seen before: 2 4 1 2 was already seen. The infinite loop is detected after the fifth block redistribution cycle, and so the answer in this example is 5.
> 
> Given the initial block counts in your puzzle input, how many redistribution cycles must be completed before a configuration is produced that has been seen before?

My input type is again a `Vector Int`:

    import Data.Vector.Unboxed (Vector, (!), (!?))
    import qualified Data.Vector.Unboxed as V
    import qualified Data.Vector.Unboxed.Mutable as M

This time, every `step` consists of three operations:

  1. Find the maximum memory block
  2. Set its value v to zero
  3. Add 1 to the next v blocks

Again our code is basically written for us, using `V.maxIndex` for 1, `V.modify` for 2, and `V.accum (flip ($))` for 3.

    step :: Vector Int -> Vector Int
    step vec =
      let i = V.maxIndex vec
          v = vec ! i
          vec' = V.modify (\vec' -> M.write vec' i 0) vec
      in V.accum (flip ($)) vec' [(j, (+1)) | j <- indices]

The only hard part is defining `indices`. We want to update every element from i+1 to i+v (`mod` the length of the list, since we wrap around). Utilizing list ranges, this specification is easily generated:

    indices =  map (`mod` (V.length vec)) [i+1 .. i+v]

And we are done with `step`.

Now that `step` is written, let's focus on iteration. We could just apply `step` to our input forever, but we want to stop as soon as we get a duplicate state. Checking for duplicates is the job of `Data.Set`:

    import Data.Set (Set)
    import qualified Data.Set as Set

That means it's not enough to keep track of just a vector every iteration. Every iteration we need to keep track of three things: the vector, the set, and the number of times `step` has been called (which is our answer). So our state is type `(Vector Int, Set (Vector Int), Int)`. For convenience, let's make a type synonym for that state:

    type MyState = (Vector Int, Set (Vector Int), Int)

Our state update function is pretty self-explanatory: `\(vec, set, i) -> (step vec, Set.insert vec set, i+1)`. We just need to run this until our condition ``vec `Set.member` set`` holds true. That's exactly what `until` does!

    loop :: MyState -> MyState
    loop = until (\(vec, set, i) -> vec `Set.member` set)
                 (\(vec, set, i) -> (step vec, Set.insert vec set, i+1))

Then we just need to supply the initial state and extract the answer:

    solve6 i = n where (_, _, n) = loop (i, Set.empty, 0)

## Question 2

> Out of curiosity, the debugger would also like to know the size of the loop: starting from a state that has already been seen, how many block redistribution cycles must be performed before that same state is seen again?
> 
> In the example above, 2 4 1 2 is seen again after four cycles, and so the answer in that example would be 4.
> 
> How many cycles are in the infinite loop that arises from the configuration in your puzzle input?

Now we need to know the cycle length. Although we just calculated when a cycle ends, we don't know when that cycle starts!

We need to store a sequential number for every `Vector Int` we store in our set. This means we need a Map instead of a Set! Since `Data.Set` and `Data.Map` offer almost identical functions, I'll just replace all occurrences of 'Set' with 'Map', adding an argument for `Map.insert`:

    import Data.Map.Strict (Map)
    import qualified Data.Map.Strict as Map

    loop :: MyState -> MyState
    loop = until (\(vec, map, i) -> vec `Map.member` map)
                 (\(vec, map, i) -> (step vec, Map.insert vec i map, i+1))

    solve6 i = n where (_, _, n) = loop (i, Map.empty, 0)

Running this still gives us the correct answer for Problem 1. Note how the extra argument I added for `Map.insert` is `i` -- every `Vector Int` is now stored with the number of `step` calls taken to reach that state.

Then solving this second problem is a matter of extracting the answer from our final state. We can find the `i` for the first duplicate vector with `map Map.! vec` and we can calculate the cycle length from there.

    solve6' i = n - (map Map.! vec)
      where (vec, map, n) = loop (i, Map.empty, 0)



