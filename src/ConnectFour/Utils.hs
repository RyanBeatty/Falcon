module ConnectFour.Utils where

import Data.List

-- | Taken from https://github.com/dmwit/universe
-- | gets main diagonals from a 2d matrix
-- | 1 | 2 | 3
-- | - - - - -
-- | 4 | 5 | 6   ----> [ [1], [4, 2], [7, 5, 3], [8, 6], [9] ]
-- | - - - - -
-- | 7 | 8 | 9
diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
    -- it is critical for some applications that we start producing answers
    -- before inspecting es_
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

-- | get "anti-diagonals" of a 2d matrix
-- | 1 | 2 | 3
-- | - - - - -
-- | 4 | 5 | 6   ----> [ [7], [4, 8], [1, 5, 9], [2, 6], [3] ]
-- | - - - - -
-- | 7 | 8 | 9
antiDiagonals :: [[a]] -> [[a]]
antiDiagonals = diagonals . map reverse

allDiagonals :: [[a]] -> [[a]]
allDiagonals xss = (diagonals xss) ++ (antiDiagonals xss)


replaceNth :: Int -> a -> [a] -> [a]
replaceNth n element xs = (init as) ++ [element] ++ bs
	where (as, bs) = splitAt n xs





