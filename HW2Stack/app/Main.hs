module Main where

import           Data.List
import           Data.Traversable
import           Log
import           LogAnalysis

main :: IO ()
main =  do
  allSortedLogs <- sort <$> testParse parseAll 10000 "error.log"
  errorLogs <- pure $ filter isError allSortedLogs
  severeErrorLogs <- pure $ filter isSevere allSortedLogs
  _ <-  traverse print severeErrorLogs
  return ()


isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _                          = False

isSevere:: LogMessage -> Bool
isSevere (LogMessage (Error severity) _ _) = severity >= 50
isSevere _                                 = False
