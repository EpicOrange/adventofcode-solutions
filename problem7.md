# Problem 7

## Question 1
> You offer to help, but first you need to understand the structure of these towers. You ask each program to yell out their name, their weight, and (if they're holding a disc) the names of the programs immediately above them balancing on that disc. You write this information down (your puzzle input). Unfortunately, in their panic, they don't do this in an orderly fashion; by the time you're done, you're not sure which program gave which information.
> 
> For example, if your list is the following:
> 
>     pbga (66)
>     xhth (57)
>     ebii (61)
>     havc (66)
>     ktlj (57)
>     fwft (72) -> ktlj, cntj, xhth
>     qoyq (66)
>     padx (45) -> pbga, havc, qoyq
>     tknk (41) -> ugml, padx, fwft
>     jptl (61)
>     ugml (68) -> gyxo, ebii, jptl
>     gyxo (61)
>     cntj (57)
> 
> ...then you would be able to recreate the structure of the towers that looks like this:
> 
>                     gyxo
>                   /     
>              ugml - ebii
>            /      \     
>           |         jptl
>           |        
>           |         pbga
>          /        /
>     tknk --- padx - havc
>          \        \
>           |         qoyq
>           |             
>           |         ktlj
>            \      /     
>              fwft - cntj
>                   \     
>                     xhth
> 
> In this example, tknk is at the bottom of the tower (the bottom program), and is holding up ugml, padx, and fwft. Those programs are, in turn, holding up other programs; in this example, none of those programs are holding up any other programs, and are all the tops of their own towers. (The actual tower balancing in front of you is much larger.)
> 
> Before you're ready to help them, you need to make sure your information is correct. What is the name of the bottom program?

My input type is `[[String]]`, by using `lines` and `map words`. I take care to filter out symbolic characters, though:

    import Data.Char (isAlphaNum, isSpace)

    input7 :: [[String]]
    input7 = let valid x = isAlphaNum x || isSpace x
      in map (words . filter valid) . lines $ <...the puzzle input...>

This turns every line into an easy-to-process format: `(name:weight:children)`.

Now, the question asks find the root given the weight and children of every node. So look for the node that doesn't appear as a child anywhere! That must be the root.

So we want the list difference of (all nodes) and (all children). Collecting all nodes is simply `map head` on the input. Similarly, collecting all children can be done by dropping the first two elements of each line (name and weight) and flattening the result with `concat`. Then the list difference operator `\\` takes care of the rest.

    import Data.List ((\\))

    solve7 i = head $ (map head i) \\ (concat . map (drop 2) $ i)

## Question 2

> The programs explain the situation: they can't get down. Rather, they could get down, if they weren't expending all of their energy trying to keep the tower balanced. Apparently, one program has the wrong weight, and until it's fixed, they're stuck here.
> 
> For any program holding a disc, each program standing on that disc forms a sub-tower. Each of those sub-towers are supposed to be the same weight, or the disc itself isn't balanced. The weight of a tower is the sum of the weights of the programs in that tower.
> 
> In the example above, this means that for ugml's disc to be balanced, gyxo, ebii, and jptl must all have the same weight, and they do: 61.
> 
> However, for tknk to be balanced, each of the programs standing on its disc and all programs above it must each match. This means that the following sums must all be the same:
> 
> - ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
> - padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
> - fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243
> 
> As you can see, tknk's disc is unbalanced: ugml's stack is heavier than the other two. Even though the nodes above ugml are balanced, ugml itself is too heavy: it needs to be 8 units lighter for its stack to weigh 243 and keep the towers balanced. If this change were made, its weight would be 60.
> 
> Given that exactly one program is the wrong weight, what would its weight need to be to balance the entire tower?

This question is now asking us to verify that, for every node, its children's weights are all equal. The weight of each node is its own weight plus the total weight of its children. We are to give the weight for the one node where the weights don't match.

That likely means we'll need to get the weight and children of arbitrary nodes. Sounds like a job for `Map`. Let's write a function `parse` to collect our input into a `Map String (Int, [String])`:

    import Data.Map.Strict (Map, (!))
    import qualified Data.Map.Strict as Map

    parse :: [[String]] -> Map String (Int, [String])
    parse = foldr (\(n:w:ns) -> Map.insert n (read w, ns)) Map.empty

Now let's focus on iterating over this map. In this case, we're working on a tree, so recursion is inevitable. (That is, I can't see a way to do this easily by defining a step function and using combinators.)

First I'd really like to have a function that gives me the total weight of any given node. This is straightforward to write starting from the definition: the total weight of a node is the weight of the node `w` plus the total weight of its children `ws`.

    weight :: Map String (Int, [String]) -> String -> Int
    weight tree node = let (w, children) = tree ! node
                           ws = map (weight tree) children
                       in w + sum ws

To solve the actual problem, though, we need to check that the children of every node have equal total weights. It might be nice to just put in a scan on `ws` in `weights` and return, say, `Left answer` whenever we get a wrong answer, and `Right (w + sum ws)` otherwise.

Since we're now working in the `Either` monad, it might be clearer to just rewrite `weight` in `do` notation:

    answerOrWeight :: Map String (Int, [String]) -> String -> Either Int Int
    answerOrWeight tree node = do
      let (w, children) = tree ! node
      ws <- sequence $ map (answerOrWeight tree) children
      -- todo: scan ws here
      return (w + sum ws)

Note that I want to scan `ws` after getting the weight, which means we scan the leaves of the tree first and the root last. After all, if some weight is unbalanced then the weights of all its parents are also unbalanced.

Now the question is, how do we pluck out the unbalanced weight from `ws`? We could scan for nonuniqueness by checking if `length (nub ws) == 2`; but then which of the two unique weights is the unbalanced one?

I chose to `group . sort` the weights. Meaning if we had `ws = [243, 251, 243]` we'd get `group . sort $ ws = [[243, 243], [251]]`. Since `ws` has two elements, we know it contains heterogenous weights and the unbalanced weight is the one in the list of length 1. We can feed the result to a function `check` that gives our `Left <answer>` if we detect an imbalance, and the `Right <sum of ws>` otherwise.

Putting this all together:

    import Data.List (sortOn)

    scan :: [Int] -> Either Int Int
    scan = check . sortOn length . group . sort

`check` is also straightforward from the definition: `Left <answer>` if we detect an imbalance, and the `Right <sum of ws>` otherwise.

    check :: [[Int]] -> Either Int Int
    check []                      = Right 0
    check [xs]                    = Right $ sum xs
    check (unbalanced:balanced:_) = Left $ getAnswer (head unbalanced) (head balanced)

The problem is `getAnswer`: there's no way to do it with just `ws`, the total weights of children! Remember that we need to answer with the weight of a node assuming that 

We can absolve this by pairing every total weight in `ws` with its node weight `w`. So we'll need to rewrite a few things.

First, `answerOrWeight` needs to work with `Right (<node weight>, <total weight>)`. We just change the type annotation and the final line:

    answerOrWeight :: Map String (Int, [String]) -> String -> Either Int (Int, Int)
    answerOrWeight tree node = do
      let (w, children) = tree ! node
      ws <- sequence $ map (answerOrWeight tree) children
      totalWeight <- scan ws
      return (w, w + totalWeight)

Second, `scan` is now sorting `(<node weight>, <total weight>)` instead of `<total weight>`. We can bring in `sortOn snd` and ``groupBy ((==) `on` snd)`` for this task:

    import Data.List (groupBy)
    import Data.Function (on)

    scan :: [(Int, Int)] -> Either Int Int
    scan = check . sortOn length . groupBy ((==) `on` snd) . sortOn snd

Third, `check` is working on a list of `[[(Int, Int)]]`, so we `map snd` before `sum`:

    check :: [[(Int, Int)]] -> Either Int Int
    check []                      = Right 0
    check [xs]                    = Right $ (sum . map snd) xs
    check (unbalanced:balanced:_) = Left $ getAnswer (head unbalanced) (head balanced)

Now we can finally write `getAnswer`! Remember, we're returning what the bad weight *should* be. We can just do this based on the difference in total weights:

    getAnswer (badWeight, badTotalWeight) (_, goodTotalWeight) =
      badWeight + (goodTotalWeight - badTotalWeight)

We're basically done now. We just need to call `answerOrWeight` with the right arguments and extract the answer from the `Left`. Remember that we can use `solve7` from the first problem to get the root, which is what we'll start recursion on.

    solve7' i = let (Left ans) = answerOrWeight (parse i) (solve7 i) in ans

(Side note: debugging this was why I'm so reluctant to solve problems with recursive functions!)
