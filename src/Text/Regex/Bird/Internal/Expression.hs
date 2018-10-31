{-| This module contains the core syntax of regular expressions.
    It is meant ot be kept minimal so that implementations of core algorithms may also remain minimal by targeting this syntax.

    This module exports all the constructors of 'GRegex' only so that their documentation is visible.
    Constructors ending an underscore are sub-optimal, and are not meant to be used outside of this module.

    The name "'GRegex'" stands for "generalized regex"; see 'GRegex' for what makes it general.
    It is recommended that you define a type synonyms such as
    @type Regex = 'GRegex' 'Symbol' 'Text' 'Char'@
    in modules/packages where you regularly use the same type(s) during regex matching.
-}
module Text.Regex.Bird.Internal.Expression
    ( -- * Regex Syntax
      GRegex(..)
    -- ** Optimizing Patterns
    , pattern Seq
    , pattern Alt
    , pattern And
    , pattern Theta
    ) where

import Text.Regex.Bird.Internal.List
import Text.Regex.Bird.Internal.Env (Env)
import qualified Text.Regex.Bird.Internal.Env as Env


-- TODO use list for Seq_, Set for Alt_; Bot = Alt []
-- TODO maybe have an environment for capture so multiple things can be captured at once
-- TODO storing a non-deterministic environment in Theta might save some space
{-| Abstract syntax for generalized regular expressions.

    These regexes are generalized by the type of input and capturing group name.
    The capturing type of group names is given by @x@.
    Good choices optimize for speed of comparison, such as 'Data.Symbol.Symbol'.
    The type of input is given by a list-like container type @str@ storing characters of type @a@.
    A good choice for the container type will have fast pop-front and push-back, and preferably also fast comparison.
    The type of character is likely domain-driven, but fast equality testing is both useful and likely to already exist.

    Several of these constructors (those ending with underscore) are sub-optimal.
    They are not meant to be used outside of this module; use the non-underscore pattern synonyms instead.
-}
data GRegex x str a =
    -- | A pattern that does not match anything, even empty string (written @⊥@).
      Bot
    -- | A pattern matching a specific sequence of characters
    --   (written @ε@ for empty or @cA*@ for non-empty starting with @c@).
    | Str str
    -- | Sequence two patterns one after the other (written @rr'@).
    --   See 'Seq' for an optimizing version.
    | Seq_ (GRegex x str a) (GRegex x str a)
    -- | Match either of the two given patterns (written @r|r'@).
    --   See 'Alt' for an optimizing version.
    | Alt_ (GRegex x str a) (GRegex x str a)
    -- | Match both of the given patterns (written @r&r'@)
    | And_ (GRegex x str a) (GRegex x str a)
    -- | Match zero or more of the given pattern (written @r*@).
    | Star (GRegex x str a)
    -- | Match exactly when the given regex does not (written @^r@)
    | Not (GRegex x str a)
    -- TODO character classes
    -- | Match the given regex while capturing input into the capture-so-far string with the given group name
    --   (written @(?x=A*.r)@, or @(?x.r)@ when @A*@ is empty).
    --   In a user's regexes, the capture-so-far string will always be empty;
    --   it only appears non-empty as an intermediate step in pattern-matching.
    | Capture x str (GRegex x str a)
    -- | Match a string that is stored in the given capturing group (written @(=x)@).
    | Replay x
    -- | A pattern that saves the capturing groups from a (written @θ: r@).
    --   See 'Theta' for an optimizing version.
    --   This expression is not meant to appear in a user's regexes;
    --   it only appears as an intermediate step in pattern matching.
    | Theta_ (Env x str) (GRegex x str a)
    deriving (Show, Eq, Ord)


{-| Optimizing pattern for sequencing two regexes together.
    Use this in place of 'Seq_'.

    - @⊥@ in a sequence will cause the sequence to never match
    - sequencing @ε@ with anything does nothing
    - sequencing two literal sequences is the catenation of the literals
-}
pattern Seq :: (ListLike t a) => GRegex x t a -> GRegex x t a -> GRegex x t a
pattern Seq r r' <- Seq_ r r'
    where
    Seq Bot _ = Bot
    Seq _ Bot = Bot
    Seq (Str Nil) r' = r'
    Seq r (Str Nil) = r
    Seq (Str w) (Str w') = Str (w <> w')
    -- TODO capture, theta
    Seq r r' = Seq_ r r'

{-| Optimizing pattern for matching either of two regexes.
    Use this in place of 'Alt_'.

    - alternation with @⊥@ always takes the non-bottom path
-}
pattern Alt :: GRegex x t a -> GRegex x t a -> GRegex x t a
pattern Alt r r' <- Alt_ r r'
    where
    Alt Bot r' = r'
    Alt r Bot = r
    Alt r r' = Alt_ r r'

{-| Optimizing pattern for matching both of two regexes.
    Use this in place of 'And_'.

    - intersection with @⊥@ always fails
-}
pattern And :: GRegex x t a -> GRegex x t a -> GRegex x t a
pattern And r r' <- And_ r r'
    where
    And Bot r' = Bot
    And r Bot = Bot
    And r r' = And_ r r'

-- TODO optimizing patterns: Capture

{-| Optimizing pattern for saving capturing group state.
    Use this in place of 'Theta_'.

    - capturing group state is irrelevant for @⊥@
    - updating capturing group state twice is equivalent to a combined update preferring the second
-}
pattern Theta :: (Ord x) => Env x t -> GRegex x t a -> GRegex x t a
pattern Theta θ r <- Theta_ θ r
    where
    Theta θ (Theta_ θ' r) = Theta_ (θ `Env.update` θ') r
    Theta _ Bot = Bot
    -- TODO I'm not sure about this one
    -- Theta _ (Str w) = Str w
    Theta θ r = Theta_ θ r
