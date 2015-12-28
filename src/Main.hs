

import ConnectFour

main :: IO ()
main = displayBoard initialBoard

displayBoard :: Board -> IO ()
displayBoard = putStr . showBoard
