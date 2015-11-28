module CrusherEvaluator where

import CrusherWalker as CW

type Piece = Char
type Board = [[Piece]]

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
        indexedColumns = map (\line -> zip line [1..]) board
        indexedRows = zip indexedColumns [1..]
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
        flatten $ map (\(Pawn label (Point x y)) -> (fn board x y label)) playerPawns

countPossibleMoves :: Board -> Piece -> Int
countPossibleMoves board playerLabel = length $ fnPossible board playerLabel generateMoves

countPossibleJumps :: Board -> Piece -> Int
countPossibleJumps board playerLabel = length $ fnPossible board playerLabel generateJumps

--possibleCrushes :: Board -> Piece -> Piece -> Int
possibleCrushes board playerLabel rivalLabel =
    let
        rivalPawnCount = countPawns board rivalLabel
        playerPossibleMoves = fnPossible board playerLabel generatePossible

    in
        filter (\board -> countPawns board rivalLabel < rivalPawnCount) playerPossibleMoves