module Main where

import CrusherWalker as CW
import CrusherEvaluator as CE

-- print (parse "WWW--W--W----BB-BBB" 3)

crusher :: [String] -> Piece -> Piece -> Int -> Int -> [String]
crusher boardStrings playerLabel rivalLabel searchDepth boardDimension =
    let
        latestBoard = parse (head boardStrings) boardDimension
        historyBoards = map (`parse` boardDimension) (tail boardStrings)
        bestMove = findBest latestBoard playerLabel rivalLabel searchDepth historyBoards
        bestMoveString = concat bestMove
    in
    bestMoveString: boardStrings

--main = print(pawnLocations (parse "WWW--W--W----BB-BBB" 3) 'W')

--main = putStrLn (printBoards(generatePossibleMoves (parse "WWW--W--W----BB-BBB" 3) 2 2 'W'))
--main = putStrLn(printBoards[(findBest (parse "WWW--W--W----BB-BBB" 3) 'W' 'B' 3 [])])
main = print(crusher ["WWW--W-------BB-BWB","WWW--W--W----BB-BBB"] 'B' 'W' 2 3)
