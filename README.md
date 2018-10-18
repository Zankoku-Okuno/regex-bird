# Regex Bird

Regular expressions with complement, intersection, and backreference.

WARNING: This is an incomplete, exploratory library, and thus not ready even for assesment for use in production.

Regular languages are useful because they are simple to understand and parse, and that's precisely because they don't come with much power.
The programmer's idea of regexes goes way beyond regular languages, but also doesn't conveniently support common operations on regular languages.

In particular, the well-known set operations of complement and intersection are absent.
Compare the normal regex to match HTML comments
```
<!--([^-]|-[^-]|--[^>])*-->

(between the delimiters, match anything that isn't a dash,
but a dash is fine if it's not followed by a dash,
_but_ a double dash is fine if _that_ isn't followed by a close angle bracket)
```
with the syntax this library supports:
```
<!--~(.*-->.*)-->

(between the delimiters, match anything that doesn't contain the close delimiter)
(noice the tilde: `~(r)` means match anything that doesn't match `r`)
```
The simple solution to this problem is already known: use (partial) derivatives of regular expressions.

Derivative-based regex libraries restrict users to truly regular languages, rather than the larger class of clanguages recognized by pcre-style regexes.
Unfortunately, there are common situations where that's _not quite_ enough power.
In particular, I want to be able to tokenize a docstring with a single regex:
```
<<<(?delim=[^\n]*)\n
    (?text=~(.*\n(=delim).*))
\n(=delim)>>>
```
where `(?x=r)` matches `r` and stores the result in the group `x`, and `(=x)` replays the last match stored in `x`.

This library implements regular expressions using derivatives, but it also extends those derivatives to handle capturing groups and simple backreference.
This allows some context-sensitivity, but it is severely limited so it cannot recognize even context-free languages in general.
