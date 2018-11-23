module Internal.Algo.Nu (smoke) where

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
smoke = catMaybes $ run_nuSmoke <$> smokeTests_nu

run_nuSmoke :: (Regex, [[(Id, T)]]) -> Maybe (Regex, NdEnv, NdEnv)
run_nuSmoke (r, expect) =
    let out = nu Env.empty r
        good = Env.test_fromLists expect
    in if out == good
        then Nothing
        else Just (r, good, out)


smokeTests_nu :: [(Regex, [[(Id, T)]])]
smokeTests_nu =
    [ (Any, [])

    , (Bot, []) -- ⊥ does not accept empty

    , (Elem $ R.singletonRange ('a', 'c'), [])

    , (Str "", [[]]) -- ε accepts empty

     -- literals do not accept empty
    , (Str "x", [])
    , (Str "xyz", [])

    -- sequence only accepts empty if both do
    , (Seq (Str "") (Str ""), [[]])
    , (Seq (Str "") (Str "b"), [])
    , (Seq (Str "a") (Str ""), [])
    , (Seq (Str "a") (Str "b"), [])

    -- alternation accepts if either does
    , (Alt (Str "") (Str ""), [[]])
    , (Alt (Str "") (Str "b"), [[]])
    , (Alt (Str "a") (Str ""), [[]])
    , (Alt (Str "a") (Str "b"), [])

    -- intersection accepts if both do
    , (And (Str "") (Str ""), [[]])
    , (And (Str "") (Str "b"), [])
    , (And (Str "a") (Str ""), [])
    , (And (Str "a") (Str "b"), [])

    -- star always accepts empty
    , (Rep (0, Nothing) (Str ""), [[]])
    , (Rep (0, Nothing) (Str "a"), [[]])
    -- plus always accepts empty
    , (Rep (1, Nothing) (Str ""), [])
    , (Rep (1, Nothing) (Str "a"), [])
    -- repetition runs out
    , (Rep (0, Just 0) (Str ""), [[]])
    , (Rep (0, Just 0) (Str "a"), [[]])

    -- complement accepts backwardsly
    , (Not (Str ""), [])
    , (Not (Str "a"), [[]])
    , (Not Bot, [[]])

    -- capture passes through with/without group
    , (Capture "x" "" (Str ""), [[("x", "")]])
    , (Capture "x" "" (Str "a"), [])
    , (Capture "x" "a" (Str ""), [[("x", "a")]])
    , (Capture "x" "a" (Str "a"), [])

    -- alternation aggregates captures
    , (Alt (Capture "x" "a" (Str "")) (Capture "x" "b" (Str "")), [[("x", "a")], [("x", "b")]])
    -- alternation joins identical captures
    , (Alt (Capture "x" "a" (Str "")) (Capture "x" "a" (Str "")), [[("x", "a")]])
    
    -- sequence combines captures
    , (Seq (Capture "x" "a" (Str "")) (Capture "y" "b" (Str "")), [[("x", "a"), ("y", "b")]])
    -- sequence overwrites captures in same variable
    , (Seq (Capture "x" "a" (Str "")) (Capture "x" "b" (Str "")), [[("x", "b")]])

    -- replay fails when capture undefined
    , (Replay "x", [])
    -- replay fails when capture is non-empty
    , (Seq (Capture "x" "a" (Str "")) (Replay "x"), [])
    -- replay succeeds when capture is empty
    , (Seq (Capture "x" "" (Str "")) (Replay "x"), [[]])

    -- state residue only updates state
    , (Theta Env.empty (Str ""), [[]])
    , (Theta Env.empty (Str "a"), [])
    , (Theta (Env.empty `Env.insert` ("1", "hi")) (Str ""), [[("1", "hi")]])
    , (Theta (Env.empty `Env.insert` ("1", "hi")) (Str "a"), [])
    -- multiple state updates stack and prefer rightmost
    , (Theta (Env.empty `Env.insert` ("1", "hi")) $
        Theta (Env.empty `Env.insert` ("2", "bye")) $ Str "", [[("1", "hi"), ("2", "bye")]])
    , (Theta (Env.empty `Env.insert` ("1", "hi")) $
        Theta (Env.empty `Env.insert` ("1", "bye")) $ Str "", [[("1", "bye")]])
    -- state update is destroyed by bottom
    , (Theta (Env.empty `Env.insert` ("1", "hi")) Bot, [])

    -- capturing groups do not escape star
    , (Rep (0, Nothing) (Theta (Env.empty `Env.insert` ("1", "boo!")) (Str "")), [[]])
    ]
