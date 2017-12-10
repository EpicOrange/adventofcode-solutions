# Problem 4

## Question 1

> To ensure security, a valid passphrase must contain no duplicate words.
> 
> For example:
> 
> - aa bb cc dd ee is valid.
> - aa bb cc dd aa is not valid - the word aa appears more than once.
> - aa bb cc dd aaa is valid - aa and aaa count as different words.
> The system's full passphrase list is available as your puzzle input. How many passphrases are valid?

Here's my input type (obtained with `map (map words) . lines` on the input string):

    input4 :: [[String]]

All the question asks is to find the number of valid passphrases. Translated to Haskell that's just '`filter` then `length`'.

    solve4 i = length . filter valid

A line is valid if it contains no duplicates, i.e. `xs == nub xs`

    solve4 i = length . filter (\xs -> xs == nub xs)

## Question 2

> For added security, yet another system policy has been put in place. Now, a valid passphrase must contain no two words that are anagrams of each other - that is, a passphrase is invalid if any word's letters can be rearranged to form any other word in the passphrase.
> 
> For example:
> 
> - abcde fghij is a valid passphrase.
> - abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
> - a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
> - iiii oiii ooii oooi oooo is valid.
> - oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.
> Under this new system policy, how many passphrases are valid?

It's the same problem! Except now we want uniquely-lettered words instead of unique words. Instead of comparing for equals, we compare for equals after sorting every word:

    solve4' = solve4 . map (map sort)