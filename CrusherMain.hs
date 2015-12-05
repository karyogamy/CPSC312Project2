module CrusherMain where

import CrusherWalker as CW
import CrusherEvaluator as CE

-- Main Crusher Function

-- This function will take in the history of board states and find the best possible move for the given playerLabel
-- using Minimax, and return with the best move prepended to the given history board list.

-- Inputs:
--  boardStrings    : A list of board state history in string, with the latest board state as head of the list.
--  playerLabel     : The pawn label (char) of the player
--  rivalLabel      : The pawn label (char) of the enemy
--  searchDepth     : Determines how deep the minimax tree will search for the best result
--  boardDimension  : The dimension/size of the board on any of its edge
crusher :: [String] -> Piece -> Piece -> Int -> Int -> [String]
crusher boardStrings playerLabel rivalLabel searchDepth boardDimension =
    let
        latestBoard = parse (head boardStrings) boardDimension
        historyBoards = map (`parse` boardDimension) (tail boardStrings)
        bestMove = findBest latestBoard playerLabel rivalLabel searchDepth historyBoards
        bestMoveString = concat bestMove
    in
    bestMoveString: boardStrings

-- Here's a sample run of crusher
main = print(crusher ["WWW--W-------BB-BWB","WWW--W--W----BB-BBB"] 'B' 'W' 2 3)
