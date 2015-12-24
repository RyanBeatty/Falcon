import Test.Tasty

import TestUtils

main = defaultMain tests

-- | Entire test suite
tests :: TestTree
tests = testGroup "Tests" [utilsTests]




