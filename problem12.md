# Problem 12

## Question 1

> You walk through the village and record the ID of each program and the IDs with which it can communicate directly (your puzzle input). Each program has one or more programs with which it can communicate, and these pipes are bidirectional; if 8 says it can communicate with 11, then 11 will say it can communicate with 8.
> 
> You need to figure out how many programs are in the group that contains program ID 0.
> 
> For example, suppose you go door-to-door like a travelling salesman and record the following list:
> 
> - 0 <-> 2
> - 1 <-> 1
> - 2 <-> 0, 3, 4
> - 3 <-> 2, 4
> - 4 <-> 2, 3, 6
> - 5 <-> 6
> - 6 <-> 4, 5
> 
> In this example, the following programs are in the group that contains program ID 0:
> 
> - Program 0 by definition.
> - Program 2, directly connected to program 0.
> - Program 3 via program 2.
> - Program 4 via program 2.
> - Program 5 via programs 6, then 4, then 2.
> - Program 6 via programs 4, then 2.
> 
> Therefore, a total of 6 programs are in this group; all but program 1, which has a pipe that connects it to itself.
> 
> How many programs are in the group that contains program ID 0?

I took out punctuation so the input is type `[[Int]]` with the format:

    [
      [0, 2],
      [1, 1],
      [2, 0, 3, 4],
      ...
    ]

This is a union find problem so I just took the package [`data-partition`](https://hackage.haskell.org/package/data-partition-0.3.0.0/docs/Data-Partition.html) to solve it for me. After transforming the input to `[Set Int]`, I create disjoint sets with the package's `fromSets` function. Then I just take the size of the set for `0`.

    solve12 :: [[Int]] -> Int
    solve12 = Set.size . flip find 0 . fromSets . map Set.fromList

## Question 2

> A group is a collection of programs that can all communicate via pipes either directly or indirectly. The programs you identified just a moment ago are all part of the same group. Now, they would like you to determine the total number of groups.
> 
> In the example above, there were 2 groups: one consisting of programs 0,2,3,4,5,6, and the other consisting solely of program 1.
> 
> How many groups are there in total?

Now we need to find the number of sets.

The package provides `nontrivialSets` which gives a list of sets with more than one element. I take `length` of that and the just count the self-loops by filtering the input:

    solve12' i = (+selfLoops) . length . nontrivialSets . fromSets $ map Set.fromList i
      where selfLoops = length . filter (\[x,y] -> x == y) . filter (\xs -> length xs == 2) $ i
