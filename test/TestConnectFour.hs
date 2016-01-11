module TestConnectFour (connectFourTests) where
import TestConnectFour.TestBoard (boardTests)
import TestConnectFour.TestPiece (pieceTests)
import TestConnectFour.TestUtils (utilsTests)
--import TestConnectFour.TestGameState (gameStateTests)

connectFourTests = [
      boardTests
    , pieceTests
    , utilsTests
    ]

