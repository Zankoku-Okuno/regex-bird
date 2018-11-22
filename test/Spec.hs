module Main where

import System.Exit

import qualified Internal.Algo.Nu
import qualified Internal.Algo.Deriv
import qualified Match
import qualified Internal.Parse


main :: IO ()
main = do
    putStrLn "Running tests..."
    report "Smoke nu" Internal.Algo.Nu.smoke
    report "Smoke deriv" Internal.Algo.Deriv.smoke
    report "Smoke match" Match.smoke
    report "Smoke parse" Internal.Parse.smoke
    putStrLn "OK"

report :: (Show err) => String -> [err] -> IO ()
report desc [] = putStrLn $ desc ++ ": ok"
report desc errs = do
    putStrLn $ desc ++ ": failure"
    print `mapM` errs
    putStrLn "FAIL"
    exitFailure
