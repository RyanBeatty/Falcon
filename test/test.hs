import Test.Tasty

import TestConnectFour
import TestAI

main = defaultMain tests

-- | Entire test suite
tests :: TestTree
tests = testGroup "Tests" [ 
	  utilsTests
    , boardTests
    , pieceTests
    , mctsTests
    ]




