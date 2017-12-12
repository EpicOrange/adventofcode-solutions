# Problem 3

## Question 1

> Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:
>
> ```
> 17  16  15  14  13
> 18   5   4   3  12
> 19   6   1   2  11
> 20   7   8   9  10
> 21  22  23---> ...
> ```
>
> While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.
> 
> For example:
> 
> - Data from square 1 is carried 0 steps, since it's at the access port.
> - Data from square 12 is carried 3 steps, such as: down, left, left.
> - Data from square 23 is carried only 2 steps: up twice.
> - Data from square 1024 must be carried 31 steps.
> 
> How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?

In solving this I noticed that the bottom right corner of the spiral forms a familiar sequence:

    1 9 25 ...

Which makes sense, since a size n * n spiral will end with the number n * n.

This was the basis for my strategy which was to find the first odd square greater than my input:

    n = head . dropWhile ((<=i) . (^2)) $ iterate (+2) 1
    -- n^2 = first odd square >i
    -- n = side length of spiral

Since the input must be somewhere on the outside of the size n^2 spiral, its Manhattan distance from the center must be the radius of the spiral plus its distance to the middle of one side.

    r = n `div` 2 -- radius of spiral
    x = (n^2 - i) `mod` n -- position of the answer on one side, [0..n)
    d = abs $ r - x -- distance to the middle (r)

Now we have our answer:

    solve3 i = r + d
      where n = head . dropWhile ((<=i) . (^2)) $ iterate (+2) 1
            r = n `div` 2
            x = (n^2 - i) `mod` n
            d = abs $ r - x


## Question 2

> As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.
> 
> So, the first few squares' values are chosen as follows:
> 
> - Square 1 starts with the value 1.
> - Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
> - Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
> - Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
> - Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
> - Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:
> 
> ```
> 147  142  133  122   59
> 304    5    4    2   57
> 330   10    1    1   54
> 351   11   23   25   26
> 362  747  806--->   ...
> ```
> 
> What is the first value written that is larger than your puzzle input?

Now we actually have to construct a spiral, but luckily the spiral probably will not need to contain hundreds of thousands of elements this time.

A really simple representation of a spiral is a matrix `[[Int]]` which is what I used. A key insight here is that updating the matrix only requires reading the one row, i.e.

    [[        ]  <-- making this row
     [10  1  1], <-- only requires information from this row
     [ 5  4  2]]

This means we can construct a spiral by repeatedly rotating the matrix and constructing a new row. Rotating a matrix anticlockwise is the operation `reverse . transpose`. (Note that I decided to make this a clockwise spiral, which should give the same answer.)

    update :: [[Int]] -> [[Int]]
    update mat = (makeRow r):mat'
      where mat'@(r:_) = (reverse . transpose) mat

First let's figure out how to make a new row from a previous row. Every element in a new row is equal to the sum of the three elements below it plus the element before it. Summing three elements below an element is possible by zipping three shifted versions of the same row:

    neighborSums xs = zipWith3 add3 (0:xs) xs (tail xs ++ [0])
      where add3 a b c = a+b+c

For example, with xs = [10 1 1] we would get:

    [ 0 10 1 1] <-- (0:xs)
    [10  1 1] <-- xs
    [ 1  1 0] <-- (tail xs ++ [0])
    -------------------
    [11 12 1] <-- zipWith3 add3 (0:xs) xs (tail xs ++ [0])

The preceding element is also a neighbor we need to consider, but isn't included with `neighborSums`. But now we can just take the prefix sums with `scanl1 (+)`, effectively including preceding elements. This gives our `makeRow` function:

    makeRow = scanl1 (+) . neighborSums

With our function `update` complete, we can use `until (condition) update [[1]]` to get a matrix using any condition. We'll need to search the resulting spiral for our answer (i.e. the first element greater than our input), so we should stop when the matrix has a number greater than our input. So just check if the last element of the first row `>= i`:

    spiral i = until (\m -> last (head m) >= i) update [[1]]

Now we can search the first row of the spiral for our solution and call it done!

    solve3' i = head $ dropWhile (<=i) (head $ spiral i)
