# Problem 13

## Question 1

> For example, suppose you've recorded the following:
> 
>     0: 3
>     1: 2
>     4: 4
>     6: 4
> 
> This means that there is a layer immediately inside the firewall (with range 3), a second layer immediately after that (with range 2), a third layer which begins at depth 4 (with range 4), and a fourth layer which begins at depth 6 (also with range 4). Visually, it might look like this:
> 
>      0   1   2   3   4   5   6
>     [ ] [ ] ... ... [ ] ... [ ]
>     [ ] [ ]         [ ]     [ ]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
> Within each layer, a security scanner moves back and forth within its range. Each security scanner starts at the top and moves down until it reaches the bottom, then moves up until it reaches the top, and repeats. A security scanner takes one picosecond to move one step. Drawing scanners as S, the first few picoseconds look like this:
> 
>     Picosecond 0:
>      0   1   2   3   4   5   6
>     [S] [S] ... ... [S] ... [S]
>     [ ] [ ]         [ ]     [ ]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
>     Picosecond 1:
>      0   1   2   3   4   5   6
>     [ ] [ ] ... ... [ ] ... [ ]
>     [S] [S]         [S]     [S]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
>     Picosecond 2:
>      0   1   2   3   4   5   6
>     [ ] [S] ... ... [ ] ... [ ]
>     [ ] [ ]         [ ]     [ ]
>     [S]             [S]     [S]
>                     [ ]     [ ]
> 
>     Picosecond 3:
>      0   1   2   3   4   5   6
>     [ ] [ ] ... ... [ ] ... [ ]
>     [S] [S]         [ ]     [ ]
>     [ ]             [ ]     [ ]
>                     [S]     [S]
> 
> Your plan is to hitch a ride on a packet about to move through the firewall. The packet will travel along the top of each layer, and it moves at one layer per picosecond. Each picosecond, the packet moves one layer forward (its first move takes it into layer 0), and then the scanners move one step. If there is a scanner at the top of the layer as your packet enters it, you are caught. (If a scanner moves into the top of its layer while you are there, you are not caught: it doesn't have time to notice you before you leave.) If you were to do this in the configuration above, marking your current position with parentheses, your passage through the firewall would look like this:
> 
>     Initial state:
>      0   1   2   3   4   5   6
>     [S] [S] ... ... [S] ... [S]
>     [ ] [ ]         [ ]     [ ]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
>     Picosecond 0:
>      0   1   2   3   4   5   6
>     (S) [S] ... ... [S] ... [S]
>     [ ] [ ]         [ ]     [ ]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
>      0   1   2   3   4   5   6
>     ( ) [ ] ... ... [ ] ... [ ]
>     [S] [S]         [S]     [S]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
> 
>     Picosecond 1:
>      0   1   2   3   4   5   6
>     [ ] ( ) ... ... [ ] ... [ ]
>     [S] [S]         [S]     [S]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
>      0   1   2   3   4   5   6
>     [ ] (S) ... ... [ ] ... [ ]
>     [ ] [ ]         [ ]     [ ]
>     [S]             [S]     [S]
>                     [ ]     [ ]
> 
> 
>     Picosecond 2:
>      0   1   2   3   4   5   6
>     [ ] [S] (.) ... [ ] ... [ ]
>     [ ] [ ]         [ ]     [ ]
>     [S]             [S]     [S]
>                     [ ]     [ ]
> 
>      0   1   2   3   4   5   6
>     [ ] [ ] (.) ... [ ] ... [ ]
>     [S] [S]         [ ]     [ ]
>     [ ]             [ ]     [ ]
>                     [S]     [S]
> 
> 
>     Picosecond 3:
>      0   1   2   3   4   5   6
>     [ ] [ ] ... (.) [ ] ... [ ]
>     [S] [S]         [ ]     [ ]
>     [ ]             [ ]     [ ]
>                     [S]     [S]
> 
>      0   1   2   3   4   5   6
>     [S] [S] ... (.) [ ] ... [ ]
>     [ ] [ ]         [ ]     [ ]
>     [ ]             [S]     [S]
>                     [ ]     [ ]
> 
> 
>     Picosecond 4:
>      0   1   2   3   4   5   6
>     [S] [S] ... ... ( ) ... [ ]
>     [ ] [ ]         [ ]     [ ]
>     [ ]             [S]     [S]
>                     [ ]     [ ]
> 
>      0   1   2   3   4   5   6
>     [ ] [ ] ... ... ( ) ... [ ]
>     [S] [S]         [S]     [S]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
> 
>     Picosecond 5:
>      0   1   2   3   4   5   6
>     [ ] [ ] ... ... [ ] (.) [ ]
>     [S] [S]         [S]     [S]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
>      0   1   2   3   4   5   6
>     [ ] [S] ... ... [S] (.) [S]
>     [ ] [ ]         [ ]     [ ]
>     [S]             [ ]     [ ]
>                     [ ]     [ ]
> 
> 
>     Picosecond 6:
>      0   1   2   3   4   5   6
>     [ ] [S] ... ... [S] ... (S)
>     [ ] [ ]         [ ]     [ ]
>     [S]             [ ]     [ ]
>                     [ ]     [ ]
> 
>      0   1   2   3   4   5   6
>     [ ] [ ] ... ... [ ] ... ( )
>     [S] [S]         [S]     [S]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
> In this situation, you are caught in layers 0 and 6, because your packet entered the layer when its scanner was at the top when you entered it. You are not caught in layer 1, since the scanner moved into the top of the layer once you were already there.
> 
> The severity of getting caught on a layer is equal to its depth multiplied by its range. (Ignore layers in which you do not get caught.) The severity of the whole trip is the sum of these values. In the example above, the trip severity is `0*3 + 6*4 = 24`.
> 
> Given the details of the firewall you've recorded, if you leave immediately, what is the severity of your whole trip?

Given the depth and range of a scanner we can figure out whether or not we hit it. We need to know if the scanner is at position 0 after *depth* seconds. The period of a scanner is *2(range-1)* so we can just check if ``depth `mod` (2 * (range - 1))`` is zero.

The severity is `depth * range` if we hit the scanner and `0` otherwise.

    severity :: (Int, Int) -> Int
    severity (depth, range)
      | depth `mod` (2 * (range - 1)) == 0 = depth * range
      | otherwise                          = 0

Now the problem reduces to a simple `sum . map`:

    solve13 :: [(Int, Int)] -> Int
    solve13 = sum . map severity

We just need to parse the input into the type `[[Int]]`. I use `splitOn` from the package `split`:

    import Data.List.Split (splitOn)

    input13 :: [(Int, Int)]
    input13 = map ((\[l,r] -> (l,r)) . map read . splitOn ": ") . lines $ <input string>

## Question 2

> Now, you need to pass through the firewall without being caught - easier said than done.
> 
> You can't control the speed of the packet, but you can delay it any number of picoseconds. For each picosecond you delay the packet before beginning your trip, all security scanners move one step. You're not in the firewall during this time; you don't enter layer 0 until you stop delaying the packet.
> 
> In the example above, if you delay 10 picoseconds (picoseconds 0 - 9), you won't get caught:
> 
>     State after delaying:
>      0   1   2   3   4   5   6
>     [ ] [S] ... ... [ ] ... [ ]
>     [ ] [ ]         [ ]     [ ]
>     [S]             [S]     [S]
>                     [ ]     [ ]
> 
>     Picosecond 10:
>      0   1   2   3   4   5   6
>     ( ) [S] ... ... [ ] ... [ ]
>     [ ] [ ]         [ ]     [ ]
>     [S]             [S]     [S]
>                     [ ]     [ ]
> 
>      0   1   2   3   4   5   6
>     ( ) [ ] ... ... [ ] ... [ ]
>     [S] [S]         [S]     [S]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
> 
>     Picosecond 11:
>      0   1   2   3   4   5   6
>     [ ] ( ) ... ... [ ] ... [ ]
>     [S] [S]         [S]     [S]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
>      0   1   2   3   4   5   6
>     [S] (S) ... ... [S] ... [S]
>     [ ] [ ]         [ ]     [ ]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
> 
>     Picosecond 12:
>      0   1   2   3   4   5   6
>     [S] [S] (.) ... [S] ... [S]
>     [ ] [ ]         [ ]     [ ]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
>      0   1   2   3   4   5   6
>     [ ] [ ] (.) ... [ ] ... [ ]
>     [S] [S]         [S]     [S]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
> 
>     Picosecond 13:
>      0   1   2   3   4   5   6
>     [ ] [ ] ... (.) [ ] ... [ ]
>     [S] [S]         [S]     [S]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
>      0   1   2   3   4   5   6
>     [ ] [S] ... (.) [ ] ... [ ]
>     [ ] [ ]         [ ]     [ ]
>     [S]             [S]     [S]
>                     [ ]     [ ]
> 
> 
>     Picosecond 14:
>      0   1   2   3   4   5   6
>     [ ] [S] ... ... ( ) ... [ ]
>     [ ] [ ]         [ ]     [ ]
>     [S]             [S]     [S]
>                     [ ]     [ ]
> 
>      0   1   2   3   4   5   6
>     [ ] [ ] ... ... ( ) ... [ ]
>     [S] [S]         [ ]     [ ]
>     [ ]             [ ]     [ ]
>                     [S]     [S]
> 
> 
>     Picosecond 15:
>      0   1   2   3   4   5   6
>     [ ] [ ] ... ... [ ] (.) [ ]
>     [S] [S]         [ ]     [ ]
>     [ ]             [ ]     [ ]
>                     [S]     [S]
> 
>      0   1   2   3   4   5   6
>     [S] [S] ... ... [ ] (.) [ ]
>     [ ] [ ]         [ ]     [ ]
>     [ ]             [S]     [S]
>                     [ ]     [ ]
> 
> 
>     Picosecond 16:
>      0   1   2   3   4   5   6
>     [S] [S] ... ... [ ] ... ( )
>     [ ] [ ]         [ ]     [ ]
>     [ ]             [S]     [S]
>                     [ ]     [ ]
> 
>      0   1   2   3   4   5   6
>     [ ] [ ] ... ... [ ] ... ( )
>     [S] [S]         [S]     [S]
>     [ ]             [ ]     [ ]
>                     [ ]     [ ]
> 
> Because all smaller delays would get you caught, the fewest number of picoseconds you would need to delay to get through safely is 10.
> 
> What is the fewest number of picoseconds that you need to delay the packet to pass through the firewall without being caught?

We could just try every delay possible until it works.

Adding to the range of every scanner effectively simulates a delay for that scanner. Then we can just check `severity scanner == 0` to check if we successfully pass a scanner. The specialized fold `all` lets us check this condition on all scanners:

    successful :: Int -> [(Int, Int)] -> Bool
    successful delay = all (\(d, r) -> severity (d+delay, r) == 0)

We'll search the set of all delays `[0..]` for the first successful delay.

    solve13' i = head $ filter (flip successful i) [0..]

(Remember, `head . filter` is the same as `fromJust . find`!)