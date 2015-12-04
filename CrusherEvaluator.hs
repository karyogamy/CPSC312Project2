module CrusherEvaluator where

import CrusherWalker as CW

type Piece = Char
type Board = [[Piece]]

data State = WINS | LOSES | ONGOING deriving (Eq)

data Pawn = Pawn {
    label :: Piece,
    coord :: Point
} deriving (Show)

countPawns :: Board -> Piece -> Int
countPawns board playerLabel =
    let
        concatenatedBoard = concat board
    in
        length $ filter (== playerLabel) concatenatedBoard

boardLocations :: Board -> [Pawn]
boardLocations board =
    let
        indexedColumns = map (\line -> zip line [0..]) board
        indexedRows = zip indexedColumns [0..]
    in
        flatten [ map (\(piece, col) -> (Pawn piece (Point col row))) line | (line, row) <- indexedRows]
{-
        flatten $ map (\(line, row) ->
                    map (\(piece, col) -> (Pawn piece (Point col row))) line)
                    indexedRows-}

pawnLocations :: Board -> Piece -> [Pawn]
pawnLocations board playerLabel =
    filter (\(Pawn label _) -> label == playerLabel) (boardLocations board)

fnPossible :: Board -> Piece -> (Board -> Int -> Int -> Piece -> [Board]) -> [Board]
fnPossible board playerLabel fn =
    let
        playerPawns = pawnLocations board playerLabel
    in
        flatten $ map (\(Pawn playerPawns (Point x y)) -> (fn board x y playerPawns)) playerPawns

possibleCrushes :: Board -> Piece -> Piece -> [Board]
possibleCrushes board playerLabel rivalLabel =
    let
        rivalPawnCount = countPawns board rivalLabel
        playerPossibleMoves = fnPossible board playerLabel generateLeaps
    in
        filter (\board -> countPawns board rivalLabel < rivalPawnCount) playerPossibleMoves

countPossibleCrushes :: Board -> Piece -> Piece -> Int
countPossibleCrushes board playerLabel rivalLabel =
    length $ possibleCrushes board playerLabel rivalLabel

countPossibleMoves :: Board -> Piece -> [Board] -> Int
countPossibleMoves board playerLabel historyBoards = length $ generateMoves board playerLabel historyBoards

generateMoves :: Board -> Piece -> [Board] -> [Board]
generateMoves board playerLabel historyBoards =
    let 
        distinctPossibleBoards = nub $ fnPossible board playerLabel generatePossibleMoves
    in
        filter (`notElem` historyBoards) distinctPossibleBoards

state :: Board -> Piece -> Piece -> [Board] -> State
state board playerLabel rivalLabel historyBoards
    | playerPawnCount == 1  = LOSES
    | playerPossible == 0   = LOSES
    | rivalPawnCount == 1   = WINS
    | rivalPossible == 0    = WINS
    | otherwise             = ONGOING
    where
        playerPawnCount = countPawns board playerLabel
        playerPossible = countPossibleMoves board playerLabel historyBoards
        rivalPawnCount  = countPawns board rivalLabel
        rivalPossible = countPossibleMoves board rivalLabel historyBoards

evaluate :: Board -> Piece -> Piece -> [Board] -> Int
evaluate board playerLabel rivalLabel historyBoards =
    let
        pawnScore = countPawns board playerLabel
        nextMoveScore = countPossibleMoves board playerLabel historyBoards
        nextCrushScore = countPossibleCrushes board playerLabel rivalLabel
    in
        pawnScore + nextMoveScore + nextCrushScore * 12

generateScore :: Board -> Piece -> Piece -> Int -> Bool -> [Board] -> Int
generateScore board playerLabel rivalLabel level isMinLevel historyBoards
    | playerState == WINS   = 1000
    | playerState == LOSES  = -1000
    | level <= 0            = evaluate board playerLabel rivalLabel historyBoards
    | otherwise             = score
    where 
        newHistory = historyBoards ++ [board]
        playerState = state board playerLabel rivalLabel newHistory
        possibleBoards = generateMoves board playerLabel newHistory

        -- In min level, the score is calculated by taking the maximum of all children
        -- In max level, the score is calculated by taking the minimum of all children
        minmaxFn
            | isMinLevel    = maximum
            | otherwise     = minimum
        score = minmaxFn (map (\onePossibleBoard -> (generateScore onePossibleBoard rivalLabel playerLabel (level-1) (not(isMinLevel)) newHistory)) possibleBoards)

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
                    ((generateScore firstMove rivalLabel playerLabel (level - 1) True historyBoards), firstMove)) firstPossibleMoves

        -- Since first move is in min level
        -- the optimal move is the one with minimum score
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
