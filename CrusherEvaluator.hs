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

countPossibleMoves :: Board -> Piece -> Int
countPossibleMoves board playerLabel = length $ fnPossible board playerLabel generateMoves

countPossibleJumps :: Board -> Piece -> Int
countPossibleJumps board playerLabel = length $ fnPossible board playerLabel generateJumps

possibleCrushes :: Board -> Piece -> Piece -> [Board]
possibleCrushes board playerLabel rivalLabel =
    let
        rivalPawnCount = countPawns board rivalLabel
        playerPossibleMoves = fnPossible board playerLabel generateJumps
    in
        filter (\board -> countPawns board rivalLabel < rivalPawnCount) playerPossibleMoves

countPossibleCrushes :: Board -> Piece -> Piece -> Int
countPossibleCrushes board playerLabel rivalLabel =
    length $ possibleCrushes board playerLabel rivalLabel

generateAlternatingMoves :: Board -> Piece -> Piece -> Int -> [Board] -> [Board]
generateAlternatingMoves board playerLabel rivalLabel level historyBoards
    | level <= 0 = [board]
    | level == 1 = distinctPossibleBoards
    | otherwise = flatten $ map (\b ->
                    generateAlternatingMoves b rivalLabel playerLabel (level-1) historyBoards) filteredPossibleBoards
    where
        distinctPossibleBoards = nub $ fnPossible board playerLabel generatePossible
        filteredPossibleBoards = filter (`notElem` historyBoards) distinctPossibleBoards

state :: Board -> Piece -> Piece -> State
state board playerLabel rivalLabel
    | playerPawnCount == 1  = LOSES
    | playerPossible == 0   = LOSES
    | rivalPawnCount == 1   = WINS
    | rivalPossible == 0    = WINS
    | otherwise             = ONGOING
    where
        playerPawnCount = countPawns board playerLabel
        playerPossible = countPossibleMoves board playerLabel + countPossibleJumps board playerLabel
        rivalPawnCount  = countPawns board rivalLabel
        rivalPossible = countPossibleMoves board rivalLabel + countPossibleJumps board rivalLabel

evaluate :: Board -> Piece -> Piece -> Int
evaluate board playerLabel rivalLabel =
    let
        playerState = state board playerLabel rivalLabel
        terminationScore
            | playerState == WINS   = 1000
            | playerState == LOSES  = -1000
            | otherwise             = 0
        pawnScore = countPawns board playerLabel - countPawns board rivalLabel
        nextMoveScore = countPossibleMoves board playerLabel - countPossibleMoves board rivalLabel
        nextJumpScore = countPossibleJumps board playerLabel - countPossibleJumps board rivalLabel
        nextCrushScore = countPossibleCrushes board playerLabel rivalLabel -
                                    countPossibleCrushes board rivalLabel playerLabel
    in
        terminationScore +
        pawnScore +
        nextMoveScore +
        nextJumpScore +
        nextCrushScore * 12

findBest :: Board -> Piece -> Piece -> Int -> [Board] -> Board
findBest board playerLabel rivalLabel level historyBoards
    | level <= 0    = board
    | otherwise     = snd maximumScoreMove
    where
        firstPossibleMoves = generateAlternatingMoves board playerLabel rivalLabel 1 historyBoards

        allPossibleMoves =
            map (\firstMove ->
                    (generateAlternatingMoves firstMove rivalLabel playerLabel (level - 1) historyBoards, firstMove)) firstPossibleMoves

        allEvaluatedMoves =
            [ (maximum $ map (\move -> evaluate move playerLabel rivalLabel) moves, firstMove) | (moves, firstMove) <- allPossibleMoves ]

        maximumScoreMove =
            foldl1 (\(s, f) (acc, m) -> if acc > s then (acc, m) else (s, f)) allEvaluatedMoves

-- snatched code of nub (distinct) from GHC source
nub :: (Eq a) => [a] -> [a]
nub list =
    nub' list []
    where
        nub' [] _ = []
        nub' (x:xs) ls
            | x `elem` ls = nub' xs ls
            | otherwise = x : nub' xs (x:ls)