import Data.Maybe

import Data.Sequence (Seq)

import qualified Text.Regex.Bird.Internal.Env as Env
import Text.Regex.Bird.Internal.Expression
import Text.Regex.Bird.Internal.Algorithms

import System.Exit


type Regex = GRegex String Char
type Env = Env.Env String (Seq Char)
type NdEnv = Env.NdEnv String (Seq Char)

main :: IO ()
main = do
    -- printf DEBUG
    -- putStrLn ""
    -- print $ d Env.empty 'a' (Seq (Capture "1" "a" (Str "")) (Replay "1"))

    putStrLn "Running tests..."
    case catMaybes $ run_nuSmoke <$> smokeTests_nu of
        [] -> putStrLn "Smoke nu: ok"
        errs -> do
            putStrLn "Smoke nu: failure"
            print `mapM_` errs
            putStrLn "FAIL"
            exitFailure
    case catMaybes $ run_derivSmoke <$> smokeTests_deriv of
        [] -> putStrLn "Smoke deriv: ok"
        errs -> do
            putStrLn "Smoke deriv: failure"
            print `mapM_` errs
            putStrLn "FAIL"
            exitFailure
    putStrLn "OK"





run_nuSmoke :: (Regex, [[(String, (Seq Char))]]) -> Maybe (Regex, NdEnv, NdEnv)
run_nuSmoke (r, expect) =
    let out = nu Env.empty r
        good = Env.test_fromLists expect
    in if out == good
        then Nothing
        else Just (r, good, out)

smokeTests_nu :: [(Regex, [[(String, (Seq Char))]])]
smokeTests_nu =
    [ (Bot, []) -- ⊥ does not accept empty
    
    , (Str "", [[]]) -- ε accepts empty
    
     -- literals do not accept empty
    , (Str "x", [])
    , (Str "xyz", [])
    
    -- sequence only accepts empty if both do
    , (Seq (Str "") (Str ""), [[]])
    , (Seq (Str "") (Str "b"), [])
    , (Seq (Str "a") (Str ""), [])
    , (Seq (Str "a") (Str "b"), [])
    
    -- alternation accept is either does
    , (Alt (Str "") (Str ""), [[]])
    , (Alt (Str "") (Str "b"), [[]])
    , (Alt (Str "a") (Str ""), [[]])
    , (Alt (Str "a") (Str "b"), [])

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
    ]





run_derivSmoke :: (Regex, Char, [[(String, (Seq Char))]]) -> Maybe (Regex, NdEnv, NdEnv)
run_derivSmoke (r, c, expect) =
    let θ = Env.empty
        out = nu θ (d θ c r)
        good = Env.test_fromLists expect
    in if out == good
        then Nothing
        else Just (r, good, out)

smokeTests_deriv :: [(Regex, Char, [[(String, (Seq Char))]])]
smokeTests_deriv =
    -- various simple derivatives are ⊥
    [ (Bot, 'a', [])
    , (Str "", 'a', [])
    , (Str "x", 'a', [])

    -- accept a single character
    , (Str "a", 'a', [[]])
    , (Str "aa", 'a', []) -- because it's still expecting another 'a'

    -- TODO what smoke tests can I run on Seq?

    -- alternation accepts either
    , (Alt (Str "a") (Str "b"), 'a', [[]])
    , (Alt (Str "a") (Str "b"), 'b', [[]])
    , (Alt (Str "a") (Str "b"), 'c', [])

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
    ]
