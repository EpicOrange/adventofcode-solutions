# Problem 1

## Question 1

> The captcha requires you to review a sequence of digits (your puzzle input) and find the sum of all digits that match the next digit in the list. The list is circular, so the digit after the last digit is the first digit in the list.
> 
> For example:
> 
> - 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
> - 1111 produces 4 because each digit (all 1) matches the next.
> - 1234 produces 0 because no digit matches the next.
> - 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.
> - What is the solution to your captcha?

Here's my input type (obtained with `map digitToInt` on the input string):

    input1 :: [Int]

I like to start with the input. We're asked to compare consecutive elements of this list, which could be done by `zip`ping the list `x:xs` with a rotated list `xs ++ x`:

    pairs :: [Int] -> [(Int, Int)]
    pairs (x:xs) = zip (x:xs) (xs ++ x)

The problem asks us to only consider pairs of equal digits. So we `filter` for equal pairs with `uncurry (==) :: Eq a => (a, a) -> Bool`. Then `map fst` takes these pairs of equal digits to digits and we can collect the answer with `sum`.

    solve1 :: [Int] -> Int
    solve1 = sum . map fst . filter (uncurry (==)) . pairs

## Question 2

> Now, instead of considering the next digit, it wants you to consider the digit halfway around the circular list. That is, if your list contains 10 items, only include a digit in your sum if the digit 10/2 = 5 steps forward matches it. Fortunately, your list has an even number of elements.
> 
> For example:
> 
> - 1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.
> - 1221 produces 0, because every comparison is between a 1 and a 2.
> - 123425 produces 4, because both 2s match each other, but no other digit has a match.
> - 123123 produces 12.
> - 12131415 produces 4.

Luckily the only change in the problem is that we're considering pairs on opposite sides of the circular list instead of consecutive pairs. So we'll change our pairing function!

Our new pairing function can divide the list into halves and then `zip` the halves together. We can split a list with `splitAt`:

    pairs' :: [Int] -> [(Int, Int)]
    pairs' xs = uncurry zip $ splitAt (length xs `div` 2) xs

This gives half of our list zipped with the other half with our list. Our `solve1'` method is going to be the same as `solve1`. Except since we've compared every digit once instead of twice, we need to double the result of our sum.

    solve1' :: [Int] -> [(Int, Int)]
    solve1' = (2*) . sum . map fst . filter (uncurry (==)) . pairs'
