# Longest Increasing Subsequence

This is a Haskell implementation of a solution to the
[Longest Increasing Subsequence](https://en.wikipedia.org/wiki/Longest_increasing_subsequence) problem.
It handles UTF-8 encoded strings.

This implementation aims to have performance competitive with C.

In my machine I can find the longest increasing subsequence in a 1.5G text file in a bit over 2.6 seconds.
In the same machine, `wc -l` processes the same file in 1.3 seconds.
Newline counting is *O(n)* whilst the target problem is *O(nlogn)*, so it is expected that it would take longer,
but being close to wc is a good parameter.

