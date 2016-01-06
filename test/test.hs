import Test.Tasty

import TestConnectFour

main = defaultMain tests

-- | Entire test suite
tests :: TestTree
tests = testGroup "Tests" 
    [ utilsTests
    , boardTests
    , pieceTests
    ]




