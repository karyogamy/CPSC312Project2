module CrusherEvaluator where

import CrusherWalker as CW

type Piece = Char
type Board = [[Piece]]

data State = WINS | LOSES | ONGOING deriving (Eq)

data Pawn = Pawn {
    label :: Piece,
    coord :: Point
} deriving (Show)

-- Count the number of pawns with the given label in the given board.
countPawns :: Board -> Piece -> Int
countPawns board playerLabel =
    let
        concatenatedBoard = concat board
    in
        length $ filter (== playerLabel) concatenatedBoard

-- Map each board piece to its position in X-Y coordinates.
-- Returns a list of Pawn data structure, which contains both the label and the coordinates.
boardLocations :: Board -> [Pawn]
boardLocations board =
    let
        indexedColumns  = map (\line -> zip line [0..]) board
        indexedRows     = zip indexedColumns [0..]
    in
        flatten [ map (\(piece, col) -> (Pawn piece (Point col row))) line | (line, row) <- indexedRows]

-- Map each pawn piece with its location given the label and the board.
pawnLocations :: Board -> Piece -> [Pawn]
pawnLocations board playerLabel =
    filter (\(Pawn label _) -> label == playerLabel) (boardLocations board)

-- Generate a list of possible moves of a given label, board and move function.
-- This is a core function that produces the possible next turn possibilities of a player.
fnPossible :: Board -> Piece -> (Board -> Int -> Int -> Piece -> [Board]) -> [Board]
fnPossible board playerLabel fn =
    let
        playerPawns = pawnLocations board playerLabel
    in
        flatten $ map (\(Pawn playerPawns (Point x y)) -> (fn board x y playerPawns)) playerPawns

-- Generates a list of possible crushes a player may do to another on the next turn.
-- This function first generates all possible jumps and then filters all the possible crushes
-- by counting the difference in the number of pawns between previous and current board state.
possibleCrushes :: Board -> Piece -> Piece -> [Board]
possibleCrushes board playerLabel rivalLabel =
    let
        rivalPawnCount      = countPawns board rivalLabel
        playerPossibleMoves = fnPossible board playerLabel generateLeaps
    in
        filter (\board -> countPawns board rivalLabel < rivalPawnCount) playerPossibleMoves

-- Counts the number of crushes a player may do to the other on the next turn.
-- Uses the existing possibleCrushes function.
countPossibleCrushes :: Board -> Piece -> Piece -> Int
countPossibleCrushes board playerLabel rivalLabel = length $ possibleCrushes board playerLabel rivalLabel

-- Counts the number of all moves a player may do to the other on the next turn.
-- Uses the existing generateMoves function.
countPossibleMoves :: Board -> Piece -> [Board] -> Int
countPossibleMoves board playerLabel historyBoards = length $ generateMoves board playerLabel historyBoards

-- Generates the all possible next move for the player pawns of a given board and both player labels.
-- This function also takes in a list of history boards to filter out, in order to prevent looping state generation.
generateMoves :: Board -> Piece -> [Board] -> [Board]
generateMoves board playerLabel historyBoards =
    let 
        distinctPossibleBoards = nub $ fnPossible board playerLabel generatePossibleMoves
    in
        filter (`notElem` historyBoards) distinctPossibleBoards

-- Determines the current state of a given board and both player labels.
-- If any side has only one pawn left, that means it can no longer crush the opponent pawns.
-- Thus, this side is automatically lost, since it takes more than one pawn to reduce the opponent to one pawn.
-- If any side cannot make a new move (in respect to history states), then this side is also considered lost.
state :: Board -> Piece -> Piece -> [Board] -> State
state board playerLabel rivalLabel historyBoards
    | playerPawnCount == 1  = LOSES
    | playerPossible == 0   = LOSES
    | rivalPawnCount == 1   = WINS
    | rivalPossible == 0    = WINS
    | otherwise             = ONGOING
    where
        playerPawnCount = countPawns board playerLabel
        playerPossible  = countPossibleMoves board playerLabel historyBoards
        rivalPawnCount  = countPawns board rivalLabel
        rivalPossible   = countPossibleMoves board rivalLabel historyBoards

-- Evaluates a given board with given player labels and generate a score on the evaluation.
-- The score is in respect of the player label, not the rival.
evaluate :: Board -> Piece -> Piece -> [Board] -> Int
evaluate board playerLabel rivalLabel historyBoards =
    let
        pawnScore       = countPawns board playerLabel
        nextMoveScore   = countPossibleMoves board playerLabel historyBoards
        nextCrushScore  = countPossibleCrushes board playerLabel rivalLabel
    in
        pawnScore + nextMoveScore + nextCrushScore * 12

-- Generates the score of an initial board state by creating alternating minimax scenario.
-- Given the player labels, the depth of minimax analysis, the history board states,
-- and the level function (which maximizes the playerLabel and minimizes the rivalLabel).
-- This function is typically run by using the opponent as the playerLabel and
-- player itself as the rival in order to obtain the worst-case possibilities for the player.
generateScore :: Board -> Piece -> Piece -> Int -> Bool -> [Board] -> Int
generateScore board playerLabel rivalLabel level isMinLevel historyBoards
    | playerState == WINS   = 1000
    | playerState == LOSES  = -1000
    | level <= 0            = evaluate board playerLabel rivalLabel historyBoards
    | otherwise             = score
    where 
        newHistory      = board : historyBoards
        playerState     = state board playerLabel rivalLabel newHistory
        possibleBoards  = generateMoves board playerLabel newHistory
        -- In min level, the score is calculated by taking the maximum of all children
        -- In max level, the score is calculated by taking the minimum of all children
        minmaxFn
            | isMinLevel    = maximum
            | otherwise     = minimum
        score = minmaxFn (map (\onePossibleBoard ->
                            generateScore onePossibleBoard rivalLabel playerLabel (level-1) (not isMinLevel) newHistory)
                            possibleBoards)

-- This function finds the best possible move under the worst circumstances for playerLabel.
findBest :: Board -> Piece -> Piece -> Int -> [Board] -> Board
findBest board playerLabel rivalLabel level historyBoards
    | level <= 0    = board
    | otherwise     = snd minScoreMove
    where
        -- generate all possible first moves for player
        firstPossibleMoves = generateMoves board playerLabel historyBoards

        -- calculate score for all moves
        allEvaluatedMoves =
            map (\firstMove ->
                    (generateScore firstMove rivalLabel playerLabel (level - 1) True historyBoards, firstMove))
                    firstPossibleMoves

        -- Since first move is in min level
        -- the optimal move is the one with minimum score
        -- Tuple = (score, move)
        minScoreMove =
            foldl1 (\(s, f) (acc, m) -> if acc < s then (acc, m) else (s, f)) allEvaluatedMoves

-- snatched code of nub (distinct) from GHC source
nub :: (Eq a) => [a] -> [a]
nub list =
    nub' list []
    where
        nub' [] _ = []
        nub' (x:xs) ls
            | x `elem` ls = nub' xs ls
            | otherwise = x : nub' xs (x:ls)
