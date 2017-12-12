# Problem 11

## Question 1

> The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be found to the north, northeast, southeast, south, southwest, and northwest:
> 
>       \ n  /
>     nw +--+ ne
>       /    \
>     -+      +-
>       \    /
>     sw +--+ se
>       / s  \
> 
> You have the path the child process took. Starting where he started, you need to determine the fewest number of steps required to reach him. (A "step" means to move from the hex you are in to any adjacent hex.)
> 
> For example:
> 
> - ne,ne,ne is 3 steps away.
> - ne,ne,sw,sw is 0 steps away (back where you started).
> - ne,ne,s,s is 2 steps away (se,se).
> - se,sw,se,sw,sw is 3 steps away (s,s,sw).

Hex positions can be represented with (x,y,z) like so:

    x    z    y
     \   |   /
       \ | /
         X
       / | \
     /   |   \ 

Then `n/s` maps to `+z/-z`, `ne/sw` maps to `+y/-y`, and `nw/se` maps to `+x/-x`. But on a 2d plane `z` is the same as `x+y` so we really have two coordinates (x, y) to deal with!

So we just map moves to this traversal of the 2d plane.

    move (x, y) "n"  = (x+1, y+1)
    move (x, y) "s"  = (x-1, y-1)
    move (x, y) "ne" = (x  , y+1)
    move (x, y) "sw" = (x  , y-1)
    move (x, y) "nw" = (x+1, y  )
    move (x, y) "se" = (x-1, y  )

    toLoc = foldl' move (0, 0)

Then to return the length of the closest path to this location, we use z = x + y. 

    toPathLength (x, y)
      | x > 0 && y > 0 = x + y - min x y
      | x > 0 && y < 0 = x - y
      | x < 0 && y > 0 = y - x
      | x < 0 && y < 0 = -x - y + max x y

And we're done.

    solve11 = toPathLength . toLoc

## Question 2

> How many steps away is the furthest he ever got from his starting position?

We can solve this (albeit slowly) by taking `inits` to get all possible prefixes of the input, and solve the problem for each variation. Then maximum of that is our answer:

    solve11' = maximum . map solve11 . inits