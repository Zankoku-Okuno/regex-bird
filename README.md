# Regex Bird

Regular expressions with complement, intersection, and backreference.

WARNING: This is an incomplete, exploratory library, and thus not ready even for assessment for use in production.

Regular languages are useful because they are simple to understand and parse, and that's precisely because they don't come with much power.
The programmer's idea of regexes goes way beyond regular languages, but also doesn't conveniently support common operations on regular languages.

In particular, the well-known set operations of complement and intersection are absent.
Compare the normal regex to match HTML comments
```
<!--([^-]|-[^-]|--[^>])*-->

(between the delimiters, match anything that isn't a dash,
but a dash is fine if it's not followed by a dash,
_but_ a double dash _is_ fine if _that_ isn't followed by a close angle bracket)
```
with the syntax that derivative-based libraries can support:
```
<!--^(.*-->.*)-->

(between the delimiters, match anything that doesn't contain the close delimiter)
(notice the circumflex: `^(r)` means match anything that doesn't match `r`)
```

Although they give additional features to humans, derivative-based regex libraries restrict users to truly regular languages, rather than the larger class of languages recognized by pcre-style regexes.
This can often be seen as an advantage, since less powerful libraries can be much more efficient (running in linear time and constant space rather than exponential time and space).
Unfortunately, there are common situations where plain regular languages have _not quite_ enough power.
In particular, I want to be able to tokenize a docstring with a single regex:
```
<<<(?delim.\w+)\n(?text=^(.*\n(=delim)>>>.*))\n(=delim)>>>

(Capture anything between triple angle brackets+delimiter into the group named "text",
just as long as that text does not contain the end delimiter at the start of a line.)

(Note that `(?x.r)` matches `r` and stores the result in the group `x`.
Further, `(=x)` replays the last match stored in `x`.)
```

This library implements regular expressions using derivatives, but it also extends those derivatives to handle capturing groups and simple backreference.
This allows some context-sensitivity, but it is severely limited so it cannot recognize even context-free languages in general.





## Contributing

We welcome bug reports at [issue tracker](https://github.com/Zankoku-Okuno/regex-bird/issues).
If you have a regex that is unexpectedly (not) matching, please come pester me!
Even if it ends up just being user error, I'm still interested, since the problem will tell me how to document this library better.

If you're interested in hacking on the library, the source code is on [github](https://github.com/Zankoku-Okuno/regex-bird).
The build system uses [stack](https://docs.haskellstack.org/en/stable/README/) so that the build is isolated from system setup.
A short list of tasks that I still need to work on is given in `TODO.txt`.

I haven't settled on any code standards yet.
This is a small enough library so far that I can normalize the style of any contributions by hand.
Documentation is important, though: a request won't be merged until there's 100% haddock coverage, and the documentation explains _why_ each part of the API exists (in particular, documentation that could be guessed from the name and type is mere redundancy).
If you'd like me to write up the documentation based on the discussion of the pull request, that's fine, but it'll just take me a bit longer to merge.


### Architecture

If you're (or future me is) going to hack with the source code, it's helpful to have a roadmap for all the modules and why each exists.
Unfortunately, I haven't found a good way to do this through haddock, so I've put it here instead.

There are two major levels of implementation in regex-bird: 1) internal, core data types and algorithms, and 2) a barely full-featured, user-facing wrapper around the core.
The core implementation is minimal so that there's the smallest volume for bugs to hide in.
The user-facing implementation offers a wider range of regular expression features, but these features are easily definable as syntactic sugar in terms of the core (e.g. `r+` is equivalent to `rr*`).
Further, whereas the core implementation only does one-step or one-character operations,
the user-facing implementation ties these basic blocks together with common patterns of iteration.

#### Core Implementation

The heart of `regex-bird` is the definition of regular expression abstract syntax and the basic derivative algorithms that operate on this syntax.
Supporting this are notions of suitable input and capturing group environments.

Haskell has a wide range of data types that can represent text strings (`String`, `Text`, `Text.Lazy`, etc), other kinds of string (`[Word8]`, `ByteString`, `ByteString.Lazy`, etc), or could otherwise reasonably have regexes applied to them (`Eq a => [a]`, `Eq a => Seq a`, etc).
To accommodate this wide range of domains, our definitions abstract over these notions using the [ListLike](http://hackage.haskell.org/package/ListLike) package.
This choice is represented in `Text.Regex.Bird.Internal.List` (where we also add a few related definitions), and exposes itself in the `GRegex x t a` type, where `t` is the container type for input, and `a` is the character type.

Capturing group environments are nothing more than sets of finite maps, but because they are used in specialized ways, we have encapsulated this functionality into the `Text.Regex.Bird.Internal.Env` module.
Again, because there are multiple types available for text identifiers (`String`, `Text`, `Symbol`, etc), we've parameterized environments by a variable type, and this exposes itself to the user in the `GRegex x t a` type, where `x` is the type used for variables.
However, I've gone to some care to ensure that the `Env` module is never needed outside the `Internal` modules: it is meant purely as a check on future me using the `containers` library too widely and sloppily.

With the scaffolding out of the way, we are left with only the essential pieces of the core: regular expressions themselves.
There are essentially three parts to this implementation.

First, there is the plain representation of regular expressions in the `GRegex` constructors.
Every constructor added to this set increases the implementation volume of the core algorithms, which is the critical area for pattern-matching bugs.
Therefore, `GRegex` must be kept as small as possible.

Second, on top of the `GRegex` constructors are a number of optimizing pattern synonyms.
These are also in the `Text.Regex.Bird.Internal.Expression` module.
These synonyms are spelled the same as the (non-optimizing) basic constructors, but without a trailing underscore.
Outside of the `Expression` module, only the non-underscore patterns should be used.
These constructors optimize away obvious wastes that are commonly generated by the derivative operation, such as sequencing the empty pattern with anything, or alternating or sequencing bottom with anything, and so on.
Since these constructors are always used in preference to the basic constructors, there is a clear potential for error whenever they are altered.
Any optimization should be thoroughly checked for strict equivalence between the input and output regexes, especially since saving the capturing group state after taking a derivative is so delicate.

Third and finally, there are the core algorithms, which are implemented in `Text.Regex.Bird.Internal.Algorithms`.
Primarily, this is the single-character derivative function (`d`), which depends on the acceptance function (`nu`).
In the future, I may consider adding a "next expected character" function.
In calculating the derivative of a regex, the entire matching state must be saved in the derivative; this includes the state of capturing groups, and this is what leads to the complexity of the derivative function.
In particular, it must leave behind traces of capturing groups that were defined by empty-accepting sub-patterns.
Otherwise, the derivative is a structurally-recursive function.
To support the capturing-group-saving behavior of the derivative, the acceptance function `nu` must not only return a boolean about whether the given regex accepts empty, but also all possible capturing groups that regex would define when it accepts.
Of course, I don't actually use a boolean at all: if the regex would not accept empty, then an empty set of possibilities is returned.
The only time when `nu` uses the input capturing group for itself is when it encounters a backreference and must determine if a capturing group is empty; otherwise, `nu` is structurally recursive.

#### User-facing API

The user-facing API is performs two duties.
The first exposes the abstract syntax and matching algorithms, whereas the second provides convenience of importing.

Again we've implemented the abstract syntax in its own module `Text.Regex.Bird.Patterns`.
That module only defines patterns which delegate their work to the internal `Expression` module.
Each of these patterns additionally restrict their type with the `Regexable` typeclass so that it is not possible to create regexes that can't be used with regex algorithms.

The regex algorithms defined in `Text.Regex.Bird.Match` operate between regexes and strings.
These algorithms return all possible matches within the string, with each function varying by whether characters may be skipped on the start and/or end of the input string.
The idea with returning all possible matches is that the user should be able to define their own criteria for which matches are the best rather than have it enforced upon them.
Thankfully, Haskell is a lazy language, and these results may be consumed as a stream.
Even the `fullMatch` algorithm can produce multiple matches (as a corollary, multiple matches can be found in the same place), as capturing groups may differ even between matches of the same string.

For ease of importing, the entire generic interface is exposed in the `Text.Regex.Bird` module.
Then, because most projects will only use one form of text type (or pass on the choice), there are a number of pre-built specializations of the `GRegex` type.
These are `Text.Regex.Bird.{String,Text,Text.Lazy}` for now, but that could expand easily with demand.
