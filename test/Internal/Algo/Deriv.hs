module Internal.Algo.Deriv (smoke) where

import Data.Maybe
import Data.Text (Text)

import qualified Data.RangeSet.Map as R
import qualified Text.Regex.Bird.Internal.Env as Env
import Text.Regex.Bird.Internal.Expression
import Text.Regex.Bird.Internal.Algorithms


type Id = String
type T = Text
type C = Char
type Regex = GRegex Id T C
type Env = Env.Env Id T
type NdEnv = Env.NdEnv Id T


smoke :: [(Regex, NdEnv, NdEnv)]
smoke = catMaybes $ run_derivSmoke <$> smokeTests_deriv

run_derivSmoke :: (Regex, C, [[(Id, T)]]) -> Maybe (Regex, NdEnv, NdEnv)
run_derivSmoke (r, c, expect) =
    let θ = Env.empty
        out = nu θ (d θ c r)
        good = Env.test_fromLists expect
    in if out == good
        then Nothing
        else Just (r, good, out)


smokeTests_deriv :: [(Regex, C, [[(Id, T)]])]
smokeTests_deriv =
    -- various simple derivatives are ⊥
    [ (Bot, 'a', [])
    , (Str "", 'a', [])
    , (Str "x", 'a', [])

    -- accept a single character
    , (Any, 'a', [[]])
    , (Str "a", 'a', [[]])
    , (Str "aa", 'a', []) -- because it's still expecting another 'a'
    , (Elem $ R.singletonRange ('a', 'b'), 'a', [[]])
    , (Elem $ R.singletonRange ('a', 'b'), 'b', [[]])
    , (Elem $ R.singletonRange ('a', 'b'), 'c', [])
    , (NotElem $ R.singletonRange ('a', 'b'), 'a', [])
    , (NotElem $ R.singletonRange ('a', 'b'), 'c', [[]])

    -- TODO what smoke tests can I run on Seq?

    -- alternation accepts either
    , (Alt (Str "a") (Str "b"), 'a', [[]])
    , (Alt (Str "a") (Str "b"), 'b', [[]])
    , (Alt (Str "a") (Str "b"), 'c', [])

    -- intersection must accept both
    , (And (Str "a") (Str "a"), 'a', [[]])
    , (And (Str "a") (Str "b"), 'a', [])
    , (And (Str "b") (Str "a"), 'a', [])

    -- Star: r* equivalent to rr* when input not empty
    , (Rep (0, Nothing) (Str "a"), 'a', [[]])
    , (Rep (0, Nothing) (Str "a"), 'c', [])
    , (Rep (0, Nothing) (Alt (Str "a") (Str "b")), 'a', [[]])
    , (Rep (0, Nothing) (Alt (Str "a") (Str "b")), 'b', [[]])
    -- empty repetition only accepts empty
    , (Rep (0, Just 0) (Str "a"), 'a', [])
    -- minimum repetition must be met
    , (Rep (1, Nothing) (Str "a"), 'a', [[]])
    , (Rep (2, Nothing) (Str "a"), 'a', [])
    -- backwards repetition bounds don't match
    , (Rep (1, Just 0) (Str "a"), 'a', [])

    -- complement accepts backwardsly
    , (Not (Str "a"), 'a', [])
    , (Not (Str "b"), 'a', [[]])

    -- capture records input
    , (Capture "1" "" (Str "a"), 'a', [[("1", "a")]])

    -- sequence passes along capturing groups from nullable front pattern
    -- replay works the same as a literal
    , (Seq (Capture "1" "" (Str "")) (Replay "1"), 'a', [])
    , (Seq (Capture "1" "a" (Str "")) (Replay "1"), 'a', [[("1", "a")]])
    , (Seq (Capture "1" "b" (Str "")) (Replay "1"), 'a', [])
    , (Seq (Capture "1" "aa" (Str "")) (Replay "1"), 'a', [])

    -- state update is copied along for later replaying
    , (Theta (Env.empty `Env.insert` ("1", "a")) (Replay "1"), 'a', [[("1", "a")]])
    -- state update is transparent to derivative for further user operations
    , (Theta (Env.empty `Env.insert` ("1", "x")) (Str "a"), 'a', [[("1", "x")]])

    -- state makes it way through an expanded star
    , (Rep (0, Nothing) (Capture "1" "" $ Alt (Str "a") (Str "b")), 'a', [[("1", "a")]])
    , (Rep (0, Nothing) (Capture "1" "" $ Alt (Str "a") (Str "b")), 'b', [[("1", "b")]])
    ]
