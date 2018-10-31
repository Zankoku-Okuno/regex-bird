{-| Module defining the core algorithms for derivative-based regex pattern matching.

    For now, the only core algorithms are:

      - 'nu': return possible matching groups for an empty-accepting pattern, and
      - 'd': compute the single-character derivative of a pattern.

    None of these operations are user-facing,
    but the user-facing operations are simple iterations of these operations.

    The algorithms given here rely on prior modules for optimization.
    This keeps the implementing code clean and therefore easy to verify against the mathematics.
    Especially useful if 'Text.Regex.Bird.Internal.Expression', which has optimizing patterns.
    With these, obviously sub-optimal patterns such as @⊥ | r@, which appear commonly with these algorithms, are optimized.
-}
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
    The rest of the time, we need to pass along the environment as if pattern matching were occuring,
    since empty-accepting sub-patterns might modify the capturing gruops for later sub-patterns.

@
ν_θ(⊥)         = 0
ν_θ(ε)         = {θ}
ν_θ(c)         = 0
ν_θ(r r')      = Σ_{θ' ∈ ν_θ(r)} Σ_{θ'' ∈ ν_{θ'}(r')} {θ''}
ν_θ(r|r')      = ν_θ(r) ∪ ν_θ(r')
ν_θ(r&r')      = Σ_{θ' ∈ ν_θ(r)} Σ_{θ'' ∈ ν_{θ}(r')} {θ''}
ν_θ(^r)      { = {θ}    if ν_θ(r) = 0
             { = 0      otherwise
ν_θ(r*)        = {θ}
ν_θ(?x=A*.r)   = Σ_{θ' ∈ ν_θ(r)} θ' ∪⃯ {x ↦ A*}
ν_θ(=x)      { = {θ}   if θ(x) = ""
             { = 0     otherwise
ν_θ(θ': r)     = ν_{θ ∪⃯ θ'}(r)
@

    Because we are using a slightly more efficient encoding, some patterns below
    do not map directly onto the definition above.
    For these (so far, only 'Str'), here are the relevant corrolaries:

@
ν_θ(cA*) = 0
@
-}
nu :: (Regexable x t a) => Env x t -> GRegex x t a -> NdEnv x t
nu _ Bot = Env.no
nu θ (Str Nil) = Env.one θ
nu _ (Str _) = Env.no
nu θ (Seq r r') = Env.many [θ'' | θ' <- Env.amb (nu θ r), θ'' <- Env.amb (nu θ' r')]
nu θ (Alt r r') = Env.join (nu θ r) (nu θ r')
nu θ (And r r') = Env.many [θ' `Env.update` θ'' | θ' <- Env.amb (nu θ r), θ'' <- Env.amb (nu θ r')]
nu θ (Star r) = Env.one θ
nu θ (Not r) = if (null . Env.amb) (nu θ r) then Env.one θ else Env.no
nu θ (Capture x str r) = Env.many $ [θ' `Env.insert` (x, str) | θ' <- Env.amb (nu θ r) ]
nu θ (Replay x) = if (null <$> θ `Env.lookup` x) == Just True then Env.ok else Env.no
nu θ (Theta θ' r) = nu (θ `Env.update` θ') r


{-| Take the derivative w.r.t. a single input character.

    The derivatives are defined as follows:

@
∂_a^θ(⊥)       = ⊥
∂_a^θ(ε)       = ⊥
∂_a^θ(c)     { = ε      if a = c
             { = ⊥      otherwise
∂_a^θ(r r')    = ∂_a^θ(r)r' | Σ_{θ' ∈ ν_θ(r)} θ': ∂_a^{θ ∪⃯ θ'}(r')
∂_a^θ(r|r')    = ∂_a^θ(r) | ∂_a^θ(r')
∂_a^θ(r&r')    = ∂_a^θ(r) & ∂_a^θ(r')
∂_a^θ(r*)      = ∂_a^θ(r) r*
∂_a^θ(^r)      = ^(∂_a^θ(r))
∂_a^θ(?x=A*.r) = (?x=A*a. ∂_a^θ(r))
∂_a^θ(=x)    { = ∂_a^θ(θ(x))   if x ∈ dom(θ)
             { = ⊥             otherwise
∂_a^θ(θ': r)  = θ': ∂_a^{θ ∪⃯ θ'}(r)
@

    Because we are using a slightly more efficient encoding, some patterns below
    do not map directly onto the definition above.
    For these (so far, only 'Str'), here are the relevant corrolaries:

@
∂_a^θ(cA*) = A*    if a = c
           = ⊥     otherwise
@
-}
d :: (Regexable x t a) => Env x t -> a -> GRegex x t a -> GRegex x t a
d _ _ Bot = Bot
d _ _ (Str Nil) = Bot
d _ a (Str (c :<| w)) = if a == c then Str w else Bot
d θ a (Seq r r') = foldr Alt
    (d θ a r `Seq` r')
    [Theta θ' $ d (θ `Env.update` θ') a r' | θ' <- Env.amb (nu θ r)]
d θ a (Alt r r') = Alt (d θ a r) (d θ a r')
d θ a (And r r') = And (d θ a r) (d θ a r')
d θ a (Star r) = Seq (d θ a r) (Star r)
d θ a (Not r) = Not (d θ a r)
d θ a (Capture x w r) = Capture x (w :|> a) (d θ a r)
d θ a (Replay x) = maybe Bot (\w -> d θ a (Str w)) $ θ `Env.lookup` x
d θ a (Theta θ' r) = Theta θ' $ d (θ `Env.update` θ') a r
