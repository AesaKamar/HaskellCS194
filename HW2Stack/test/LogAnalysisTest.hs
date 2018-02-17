

import           Data.Monoid
import           Log
import           LogAnalysis
import           Test.Tasty
import           Test.Tasty.HUnit


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ parseIndividualLogs, messageTrees]



parseIndividualLogs = testGroup "Parse logs"
  [ testCase "Error" $
    parseMessage "E 2 562 help help" @?=
      LogMessage (Error 2) 562 "help help"

  -- the following test does not hold
  , testCase "Info" $
    parseMessage "I 29 la la la" @?=
      LogMessage Info 29 "la la la"

  , testCase "Unknown" $
    parseMessage "This is not in the right format" @?=
      Unknown "This is not in the right format"

  , testCase "parseAll" $
    parseAll  (mconcat $ replicate 10 "blah\n") @?=
      (replicate 10  (Unknown "blah"))

  ]

messageTrees = testGroup "Message trees"
  [ testCase "insert unknown" $
    insert (Unknown "blah") Leaf @?=
      Leaf

  ]
