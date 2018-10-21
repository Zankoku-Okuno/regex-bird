import Data.Maybe

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Text.Regex.Bird.Internal.Env as Env
import Text.Regex.Bird.Internal.Expression
import Text.Regex.Bird.Internal.Algorithms

import Text.Regex.Bird.Match

import System.Exit

type Id = String
type T = Text
type C = Char
type Regex = GRegex Id T C
type Env = Env.Env Id T
type NdEnv = Env.NdEnv Id T

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
    case catMaybes $ run_matchSmoke <$> smokeTests_match of
        [] -> putStrLn "Smoke match: ok"
        errs -> do
            putStrLn "Smoke match: failure"
            print `mapM_` errs
            putStrLn "FAIL"
            exitFailure
    putStrLn "OK"





run_nuSmoke :: (Regex, [[(Id, T)]]) -> Maybe (Regex, NdEnv, NdEnv)
run_nuSmoke (r, expect) =
    let out = nu Env.empty r
        good = Env.test_fromLists expect
    in if out == good
        then Nothing
        else Just (r, good, out)

smokeTests_nu :: [(Regex, [[(Id, T)]])]
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

    -- star always accepts empty
    , (Star (Str ""), [[]])
    , (Star (Str "a"), [[]])

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
    , (Star (Theta (Env.empty `Env.insert` ("1", "boo!")) (Str "")), [[]])
    ]





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
    , (Str "a", 'a', [[]])
    , (Str "aa", 'a', []) -- because it's still expecting another 'a'

    -- TODO what smoke tests can I run on Seq?

    -- alternation accepts either
    , (Alt (Str "a") (Str "b"), 'a', [[]])
    , (Alt (Str "a") (Str "b"), 'b', [[]])
    , (Alt (Str "a") (Str "b"), 'c', [])

    -- Star: r* equivalent to rr* when input not empty
    , (Star (Str "a"), 'a', [[]])
    , (Star (Str "a"), 'c', [])
    , (Star (Alt (Str "a") (Str "b")), 'a', [[]])
    , (Star (Alt (Str "a") (Str "b")), 'b', [[]])

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
    , (Star (Capture "1" "" $ Alt (Str "a") (Str "b")), 'a', [[("1", "a")]])
    , (Star (Capture "1" "" $ Alt (Str "a") (Str "b")), 'b', [[("1", "b")]])
    ]





run_matchSmoke :: (Regex, T, [[(Id, T)]])
                -> Maybe (Regex, [Map Id T], [Map Id T])
run_matchSmoke (r, str, expect) =
    let out = fullMatches r str
        good = Map.fromList <$> expect
    in if length out == length good -- FIXME test the actual matches
        then Nothing
        else Just (r, good, capturingGroups <$> out)

smokeTests_match :: [(Regex, T, [[(Id, T)]])]
smokeTests_match =
    -- various simple matches succeed/fail
    [ (Bot, "", [])
    , (Bot, "a", [])
    , (Bot, "as", [])

    , (Str "", "", [[]])
    , (Str "", "a", [])
    , (Str "", "aa", [])

    , (Str "a", "", [])
    , (Str "a", "a", [[]])
    , (Str "a", "aa", [])

    , (Str "aa", "", [])
    , (Str "aa", "a", [])
    , (Str "aa", "aa", [[]])

    -- alternates
    , (Alt (Str "a") (Str "b"), "a", [[]])
    , (Alt (Str "a") (Str "b"), "b", [[]])
    , (Alt (Str "a") (Str "b"), "c", [])

    -- star
    , (Star (Str "a"), "", [[]])
    , (Star (Str "a"), "a", [[]])
    , (Star (Str "a"), "aa", [[]])
    , (Star (Str "a"), "aaa", [[]])

    -- replay
    , (Seq (Capture "1" "" $ Alt (Str "a") (Str "b")) (Replay "1"), "aa", [[("1", "a")]])
    , (Seq (Capture "1" "" $ Alt (Str "a") (Str "b")) (Replay "1"), "bb", [[("1", "b")]])
    , (Seq (Capture "1" "" $ Alt (Str "a") (Str "b")) (Replay "1"), "ab", [])
    , (Seq (Capture "1" "" $ Alt (Str "a") (Str "b")) (Replay "1"), "ba", [])

    -- replay star
    , (Seq (Capture "1" "" $ Star (Str "a")) (Replay "1"), "", [[]])
    , (Seq (Capture "1" "" $ Star (Str "a")) (Replay "1"), "a", [])
    , (Seq (Capture "1" "" $ Star (Str "a")) (Replay "1"), "aa", [[]])
    , (Seq (Capture "1" "" $ Star (Str "a")) (Replay "1"), "aaa", [])
    , (Seq (Capture "1" "" $ Star (Str "a")) (Replay "1"), "aaaa", [[]])

    -- TODO more
    ]
