module Match (smoke) where

import Data.Maybe
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Regex.Bird


type Id = String
type T = Text
type C = Char
type Regex = GRegex Id T C


smoke :: [(Regex, [Map Id T], [Map Id T])]
smoke = catMaybes $ run_matchSmoke <$> smokeTests_match

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
    [ (Any, "", [])
    , (Any, "a", [[]])
    , (Any, "b", [[]])
    , (Any, "as", [])

    , (Bot, "", [])
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

    -- intersections
    , (And (Str "a") (Star $ Str "a"), "aa", [])
    , (And (Str "aa") (Star $ Str "a"), "aa", [[]])
    , (And (Str "aaa") (Star $ Str "a"), "aa", [])

    -- star
    , (Star (Str "a"), "", [[]])
    , (Star (Str "a"), "a", [[]])
    , (Star (Str "a"), "aa", [[]])
    , (Star (Str "a"), "aaa", [[]])

    -- not
    , (Not (Seq (Star Any) $ Seq (Str "b") (Star Any)), "", [[]])
    , (Not (Seq (Star Any) $ Seq (Str "b") (Star Any)), "aaa", [[]])
    , (Not (Seq (Star Any) $ Seq (Str "b") (Star Any)), "bbb", [])
    , (Not (Seq (Star Any) $ Seq (Str "b") (Star Any)), "ccc", [[]])
    , (Not (Seq (Star Any) $ Seq (Str "b") (Star Any)), "ba", [])
    , (Not (Seq (Star Any) $ Seq (Str "b") (Star Any)), "aba", [])
    , (Not (Seq (Star Any) $ Seq (Str "b") (Star Any)), "aaba", [])
    , (Not (Seq (Star Any) $ Seq (Str "b") (Star Any)), "aabaa", [])

    -- replay
    , (Seq (Capture "1" $ Alt (Str "a") (Str "b")) (Replay "1"), "aa", [[("1", "a")]])
    , (Seq (Capture "1" $ Alt (Str "a") (Str "b")) (Replay "1"), "bb", [[("1", "b")]])
    , (Seq (Capture "1" $ Alt (Str "a") (Str "b")) (Replay "1"), "ab", [])
    , (Seq (Capture "1" $ Alt (Str "a") (Str "b")) (Replay "1"), "ba", [])

    -- replay star
    , (Seq (Capture "1" $ Star (Str "a")) (Replay "1"), "", [[]])
    , (Seq (Capture "1" $ Star (Str "a")) (Replay "1"), "a", [])
    , (Seq (Capture "1" $ Star (Str "a")) (Replay "1"), "aa", [[]])
    , (Seq (Capture "1" $ Star (Str "a")) (Replay "1"), "aaa", [])
    , (Seq (Capture "1" $ Star (Str "a")) (Replay "1"), "aaaa", [[]])

    -- TODO more
    ]
