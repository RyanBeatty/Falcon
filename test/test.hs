import Test.Tasty

import TestBoard

main = defaultMain tests

-- | Entire test suite
tests :: TestTree
tests = testGroup "Tests" [boardTests]




