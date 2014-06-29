module Main where

import ABNF.Parser
import ABNF.Printer
import ABNF.RFC5234
import System.Exit

main :: IO ()
main = backAndForth

backAndForth :: IO ()
backAndForth =
  do let rl_string = show $ ppRuleList $ abnf_ruleList
         rl = parseRuleList "" rl_string
     if (abnf_ruleList == rl)
       then do putStrLn "success."
               exitSuccess
       else do putStrLn "failure."
               exitFailure
