module TestConnectFour (
          module TestConnectFour.TestBoard
        , module TestConnectFour.TestPiece
        , module TestConnectFour.TestUtils
    ) where
import TestConnectFour.TestBoard (boardTests)
import TestConnectFour.TestPiece (pieceTests)
import TestConnectFour.TestUtils (utilsTests)


