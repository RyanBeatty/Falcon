import Test.Tasty

import TestUtils
import TestBoard
import TestPiece

main = defaultMain tests

-- | Entire test suite
tests :: TestTree
tests = testGroup "Tests" 
    [ utilsTests
    , boardTests
    , pieceTests
    ]




