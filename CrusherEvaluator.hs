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
        playerPossibleMoves = fnPossible board playerLabel generatePossible
    in
        filter (\board -> countPawns board rivalLabel < rivalPawnCount) playerPossibleMoves

countPossibleCrushes :: Board -> Piece -> Piece -> Int
countPossibleCrushes board playerLabel rivalLabel =
    length $ possibleCrushes board playerLabel rivalLabel

generateAlternatingMoves :: Board -> Piece -> Piece -> Int -> [Board]
generateAlternatingMoves board playerLabel rivalLabel level
    | level <= 0 = [board]
    | level == 1 = allPossibleOnLevel
    | otherwise = flatten $ map (\b -> generateAlternatingMoves b rivalLabel playerLabel (level-1)) allPossibleOnLevel
    where
        allPossibleOnLevel = fnPossible board playerLabel generatePossible

state :: Board -> Piece -> Piece -> State
state board playerLabel rivalLabel
    | playerPawnCount == 0  = LOSES
    | rivalPawnCount == 0   = WINS
    | otherwise             = ONGOING
    where
        playerPawnCount = countPawns board playerLabel
        rivalPawnCount  = countPawns board rivalLabel

evaluate :: Board -> Piece -> Piece -> Int
evaluate board playerLabel rivalLabel =
    let
        playerState = state board playerLabel rivalLabel
        terminationScore
            | playerState == WINS   = 100
            | playerState == LOSES  = -100
            | otherwise             = 0
        pawnCountDifference = countPawns board playerLabel - countPawns board rivalLabel
        nextMoveCountDifference = countPossibleMoves board playerLabel - countPossibleMoves board rivalLabel
        nextJumpCountDifference = countPossibleJumps board playerLabel - countPossibleJumps board rivalLabel
        nextCrushCountDifference = countPossibleCrushes board playerLabel rivalLabel -
                                    countPossibleCrushes board rivalLabel playerLabel
    in
        terminationScore +
        pawnCountDifference +
        nextMoveCountDifference +
        nextJumpCountDifference +
        nextCrushCountDifference * 2

findBest board playerLabel rivalLabel level
    | level <= 0    = board
    | otherwise     = snd maximumScoreMove
    where
        firstPossibleMoves = generateAlternatingMoves board playerLabel rivalLabel 1

        allPossibleMoves =
            map (\firstMove ->
                    (generateAlternatingMoves firstMove rivalLabel playerLabel (level - 1), firstMove)) firstPossibleMoves

        allEvaluatedMoves =
            [ (map (\move -> evaluate move playerLabel rivalLabel) moves, firstMove) | (moves, firstMove) <- allPossibleMoves ]

        maximizedMoves =
            map (\(scores, firstMove) -> (maximum scores, firstMove)) allEvaluatedMoves

        maximumScoreMove =
            foldl1 (\(s, f) (acc, m) -> if acc > s then (acc, m) else (s, f)) maximizedMoves