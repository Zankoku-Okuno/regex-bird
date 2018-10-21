module Text.Regex.Bird.Internal.Expression where

import Text.Regex.Bird.Internal.List
import Text.Regex.Bird.Internal.Env (Env)
import qualified Text.Regex.Bird.Internal.Env as Env


-- TODO use list for Seq_, Set for Alt_; Bot = Alt []
-- TODO maybe have an environment for capture so multiple things can be captured at once
-- FIXME I ought to be able to generalize Seq to "stream" (a-la Parsec) so I can support Text instead of Seq Char
data GRegex x str a =
      Bot
    | Str str
    | Seq_ (GRegex x str a) (GRegex x str a)
    | Alt_ (GRegex x str a) (GRegex x str a)
    | Star (GRegex x str a)
    -- TODO complement, intersection, character classes
    | Capture x str (GRegex x str a)
    | Replay x
    | Theta_ (Env x str) (GRegex x str a) -- save capturing group state
    deriving (Show, Eq, Ord)


-- syntactic sugar
-- TODO optional, plus, bounded repetition

pattern Empty :: (ListLike t a) => GRegex x t a
pattern Empty = Str Nil

pattern Char :: (ListLike t a) => a -> GRegex x t a
pattern Char c = Str (c :<| Nil)


-- optimizing patterns

pattern Seq :: (ListLike t a) => GRegex x t a -> GRegex x t a -> GRegex x t a
pattern Seq r r' <- Seq_ r r'
    where
    Seq Bot _ = Bot
    Seq Empty r' = r'
    Seq r Empty = r
    Seq (Str w) (Str w') = Str (w <> w')
    -- TODO capture, theta
    Seq r r' = Seq_ r r'

pattern Alt :: GRegex x t a -> GRegex x t a -> GRegex x t a
pattern Alt r r' <- Alt_ r r'
    where
    Alt Bot r' = r'
    Alt r Bot = r
    Alt r r' = Alt_ r r'

-- TODO optimizing patterns: Capture

pattern Theta :: (Ord x) => Env x t -> GRegex x t a -> GRegex x t a
pattern Theta θ r <- Theta_ θ r
    where
    Theta θ (Theta_ θ' r) = Theta_ (θ `Env.update` θ') r
    -- TODO I'm not sure about these two
    -- Theta _ Bot = Bot
    -- Theta _ (Str w) = Str w
    Theta θ r = Theta_ θ r
