import Test.Tasty

import TestConnectFour
import TestAI

main = defaultMain tests

-- | Entire test suite
tests :: TestTree
tests = testGroup "Tests" $ concat [
	  connectFourTests
	, aiTests
	]




