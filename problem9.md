# Problem 9

## Question 1

> You sit for a while and record part of the stream (your puzzle input). The characters represent groups - sequences that begin with { and end with }. Within a group, there are zero or more other things, separated by commas: either another group or garbage. Since groups can contain other groups, a } only closes the most-recently-opened unclosed group - that is, they are nestable. Your puzzle input represents a single, large group which itself contains many smaller ones.
> 
> Sometimes, instead of a group, you will find garbage. Garbage begins with < and ends with >. Between those angle brackets, almost any character can appear, including { and }. Within garbage, < has no special meaning.
> 
> In a futile attempt to clean up the garbage, some program has canceled some of the characters within it using !: inside garbage, any character that comes after ! should be ignored, including <, >, and even another !.
> 
> You don't see any characters that deviate from these rules. Outside garbage, you only find well-formed groups, and garbage always terminates according to the rules above.
> 
> Here are some self-contained pieces of garbage:
> 
> - <>, empty garbage.
> - <random characters>, garbage containing random characters.
> - <<<<>, because the extra < are ignored.
> - <{!>}>, because the first > is canceled.
> - <!!>, because the second ! is canceled, allowing the > to terminate the garbage.
> - <!!!>>, because the second ! and the first > are canceled.
> - <{o"i!a,<{i<a>, which ends at the first >.
> 
> Here are some examples of whole streams and the number of groups they contain:
> 
> - {}, 1 group.
> - {{{}}}, 3 groups.
> - {{},{}}, also 3 groups.
> - {{{},{},{{}}}}, 6 groups.
> - {<{},{},{{}}>}, 1 group (which itself contains garbage).
> - {<a>,<a>,<a>,<a>}, 1 group.
> - {{<a>},{<a>},{<a>},{<a>}}, 5 groups.
> - {{<!>},{<!>},{<!>},{<a>}}, 2 groups (since all but the last > are canceled).
> 
> Your goal is to find the total score for all groups in your input. Each group is assigned a score which is one more than the score of the group that immediately contains it. (The outermost group gets a score of 1.)
> 
> - {}, score of 1.
> - {{{}}}, score of 1 + 2 + 3 = 6.
> - {{},{}}, score of 1 + 2 + 2 = 5.
> - {{{},{},{{}}}}, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
> - {<a>,<a>,<a>,<a>}, score of 1.
> - {{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
> - {{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
> - {{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.
> 
> What is the total score for all groups in your input?

Wordy parser problem! Fortunately we just have a few tasks:

  1. Ignore any character after `!`
  2. Ignore characters inside garbage `<`..`>`
  3. Parse nested groups of `{}`, where the score of a group = level of nesting of the group.
  4. Return sum of scores

These specifications don't look difficult enough for a full-blown parser so I just went with regex (package `regex`). And my input type is just the raw input `String`:

    import Text.RE.Replace
    import Text.RE.TDFA.String

    input9 :: String

First of all, we filter out all cancellations and garbage, which we can match using respective regexes `/!./` and `/<[^>]*>/`. In our regex library we specify a regex with `[re|...|]` (requires `-XQuasiQuotes`), and apply the "match all" operator `*=~`.

    {-# LANGUAGE QuasiQuotes #-}

    uncancel, ungarbage :: String -> String
    uncancel = replaceAll "" . (*=~ [re|!.|])
    ungarbage = replaceAll "" . (*=~ [re|<[^>]*>|])

After these three filters, we should be left with a string of only `'{'`, `'}'`, and `','`. We can apply `filter (/=',')` to get rid of the commas. Then, counting score can be a simple `foldl'` accumulating `(depth, total)`. Putting it all together:

    import Data.List (foldl')

    solve9 :: String -> Int
    solve9 = snd . foldl' score (0, 0) . filter (/=',') . ungarbage . uncancel
      where score (depth, total) '{' = (depth+1, total)
            score (depth, total) '}' = (depth-1, total+depth)

## Question 2

> To prove you've removed it, you need to count all of the characters within the garbage. The leading and trailing < and > don't count, nor do any canceled characters or the ! doing the canceling.
> 
> - <>, 0 characters.
> - <random characters>, 17 characters.
> - <<<<>, 3 characters.
> - <{!>}>, 2 characters.
> - <!!>, 0 characters.
> - <!!!>>, 0 characters.
> - <{o"i!a,<{i<a>, 10 characters.
>
> How many non-canceled characters are within the garbage in your puzzle input?

This time we can just apply `uncancel` but keep the garbage.

We can match all regex `/<([^>]*)>/` and collect the subexpression `[^>]*`. Our regex library reads this as `[re|<$([^>]*)>|]`, using `$(..)` to indicate subexpressions. Then `map (captureText [cp|1|]) . allMatches` gives us a list of captures, and apply `sum . map length` to get the total length of the garbage:
        
    solve9' = sum . map length . getCaptures . (*=~ [re|<$([^>]*)>|]) . uncancel
      where getCaptures = map (captureText [cp|1|]) . allMatches