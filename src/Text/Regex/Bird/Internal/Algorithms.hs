module Text.Regex.Bird.Internal.Algorithms where

import Prelude hiding (foldr, null)

import Text.Regex.Bird.Internal.List
import Text.Regex.Bird.Internal.Env (Env, NdEnv)
import qualified Text.Regex.Bird.Internal.Env as Env
import Text.Regex.Bird.Internal.Expression



{-| Check if the given regex will accept empty.

    If so, it will return any capturing groups the regex defines.
    If the regex does not define any capturing groups, that is represented with 'Env.ok'.
    If the regex will not accept empty, that is represented with 'Env.no'.

    The capturing environment is necessary for replay expressions.
    Replaying a variable will accept empty only when that variable is captured as empty.
    The rest of the time, we need to pass along the environment as if pattern matching were occuring.
-}
nu :: (Regexable x t a) => Env x t -> GRegex x t a -> NdEnv x t
-- ν_θ(⊥) = 0
nu _ Bot = Env.no
-- ν_θ(ε) = 1_θ
-- ν_θ(c) = 0
-- corollary: ν_θ(cA*) = 0
nu θ (Str Nil) = Env.one θ
nu _ (Str _) = Env.no
-- ν_θ(r r') = Σ_{θ' ∈ ν_θ(r)} Σ_{θ'' ∈ ν_{θ'}(r')} 1_{θ''}
nu θ (Seq r r') = Env.many $ [θ'' | θ' <- Env.amb (nu θ r), θ'' <- Env.amb (nu θ' r')]
-- ν_θ(r|r') = ν_θ(r) ∪ ν_θ(r')
nu θ (Alt r r') = Env.join (nu θ r) (nu θ r')
-- ν_θ(r*) = 1_θ
nu θ (Star r) = Env.one θ
-- ν_θ(x=A*. r) = Σ_{θ' ∈ ν_θ(r)} θ' ∪⃯ {x ↦ A*}
nu θ (Capture x str r) = Env.many $ [θ' `Env.insert` (x, str) | θ' <- Env.amb (nu θ r) ]
-- ν_θ(x) = 1     if θ(x) = ""
--        = 0     otherwise
nu θ (Replay x) = if (null <$> θ `Env.lookup` x) == Just True then Env.ok else Env.no
-- ν_θ(θ': r) = ν_{θ ∪⃯ θ'}(r)
nu θ (Theta θ' r) = nu (θ `Env.update` θ') r


{-| Take the derivative w.r.t. a single input character. -}
d :: (Regexable x t a) => Env x t -> a -> GRegex x t a -> GRegex x t a
-- ∂_a^θ(⊥) = ⊥
d _ _ Bot = Bot
-- ∂_a^θ(ε) = ⊥
-- ∂_a^θ(c) = ε      if a = c
--          = ⊥      otherwise
-- corollary: ∂_a^θ(cA*) = A*    if a = c
--                       = ⊥     otherwise
d _ _ (Str Nil) = Bot
d _ a (Str (c :<| w)) = if a == c then Str w else Bot
-- ∂_a^θ(r r') = ∂_a^θ(r)r' | Σ_{θ' ∈ ν_θ(r)} θ': ∂_a^{θ ∪⃯ θ'}(r')
d θ a (Seq r r') = foldr Alt
    (d θ a r `Seq` r')
    [Theta θ' $ d (θ `Env.update` θ') a r' | θ' <- Env.amb (nu θ r)]
-- ∂_a^θ(r|r') = ∂_a^θ(r) | ∂_a^θ(r')
d θ a (Alt r r') = Alt (d θ a r) (d θ a r')
-- ∂_a^θ(r*) = -- ∂_a^θ(r) r*
d θ a (Star r) = Seq (d θ a r) (Star r)
-- ∂_a^θ(x=A*. r) = (x=A*a. ∂_a^θ(r))
d θ a (Capture x w r) = Capture x (w :|> a) (d θ a r)
-- ∂_a^θ(x) = ∂_a^θ(θ(x))   if x ∈ dom(θ)
--          = 0             otherwise
d θ a (Replay x) = maybe Bot (\w -> d θ a (Str w)) $ θ `Env.lookup` x
-- ∂_a^θ(θ': r) = θ': ∂_a^{θ ∪⃯ θ'}(r)
d θ a (Theta θ' r) = Theta θ' $ d (θ `Env.update` θ') a r
