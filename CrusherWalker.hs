module CrusherWalker where

-- Returns the number of characters to take per row when parsing the raw string.
-- Takes:
--  n = The dimension of the board
-- An example would be for a board size of 3, [3, 4, 5, 4, 3].
getTakeNums :: Int -> [Int]
getTakeNums n =
    let n2temp = [ n2 | n2 <- [n,n+1..(2*n-2)]]
    in n2temp ++ [2*n-1] ++ (reverse n2temp)

-- Parses a raw string into rows
-- Takes:
--  s = A raw string, generally of the format "WWW-WW-------BB-BBB"
--  n = The dimension of the board
parse :: String -> Int -> [String]
parse s n = reverse (parse' s (getTakeNums n) 0 []) where
    parse' s [] skip ret = ret
    parse' s (h:t) skip ret = parse' s t (skip + h) ((take h (drop skip s)) : ret)

-- Interleaves spaces between characters in a string
spaceOut :: String -> String
spaceOut s = spaceOut' s "" where
    spaceOut' [] ret = ret
    spaceOut' (ch:t) ret = spaceOut' t (ret ++ [ch, ' '])

-- Formats a board into a nicely readable format.
-- Takes:
--  b = a board
--  n the board dimension
-- Example:
--  prettyFormat ["WWW", "-WW-", "-----", "-BB-", "BBB"] 3 will return a string that prints:
--  
--    W W W
--   - W W -
--  - - - - -
--   - B B -
--    B B B
prettyFormat :: [String] -> Int -> String
prettyFormat b n
    | null b = ""
    | otherwise =
        let h = (head b)
        -- Calculates the leading spaces (2n-1 - length of row), adds the spaced out string, a newline, then moves onto the next line
        in [ ' ' | _ <- [0, 1.. (2*n-1-(length h))]] ++ (spaceOut h) ++ "\n" ++ (prettyFormat (tail b) n)

-- unparses a board
flatten :: [[a]] -> [a]
flatten (h:t) = h ++ (flatten t)
flatten [] = []

-- callbacks for runAction
clear :: Char -> Char
clear c = '-'

setSlide :: Char -> Char -> Char
setSlide turn c = (if c == '-' then turn else c)

setLeap :: Char -> Char -> Char
setLeap turn c = if not (c == turn) then turn else c

-- perform fn on b @ x,y
runAction :: [String] -> Int -> Int -> (Char -> Char) -> [String]
runAction b x y fn = runLine y (runLine x fn) b

-- perfom fn on s @ x
runLine :: Int -> (a -> a) -> [a] -> [a]
runLine x fn s = runLine' x fn s [] where
    runLine' x fn s h
        | x == 0 = h ++ [fn (head s)] ++ (tail s)
        | otherwise =
            runLine' (x-1) fn (tail s) (h ++ [(head s)])

            
-- Returns the char at x,y in board
at :: [String] -> Int -> Int -> Char
at board x y = head (drop x (head (drop y board)))

-- Returns the length of the row in the board or -1
rowLength :: [String] -> Int -> Int
rowLength b y =
    if length b <= y
    then
        -1
    else
        length (head (drop y b))

-- shift calculate the x coordinate in a hex coordinate system
-- when moving upleft, upright, downleft, downright
-- y2 indicates up/down, y2 = y+1 (up) or y-1 (down)
-- left indicates left/right, left = 1 (left)
shift :: [String] -> Int -> Int -> Int -> Int -> Int
shift board x y y2 left =
    let currentRl = (rowLength board y)
        otherRl = (rowLength board y2)
    in
        if currentRl < otherRl
        then
            if left == 1
            then 
                x
            else
                (x+1)
        else
            if left == 1
            then
                (x-1)
            else
                x

-- checks if x,y is a valid coordinate on the board
validCoord :: [String] -> Int -> Int -> Bool
validCoord board x y =
    (x >= 0) && (y >= 0) && (y < (length board)) && (x < (rowLength board y))

-- try move pawn to new x and y by applying fn (setSlide or setLeap)
-- if succeed return new board
-- if fail return empty
tryMove :: [String] -> Int -> Int -> (Char -> Char) -> [[String]]
tryMove base x y fn =
    let new = runAction base x y fn
    in
        if (validCoord base x y) && not ((flatten new) == (flatten base)) then
            [new]
        else
            []

data Point = Point Int Int deriving (Show)
data Hop = Hop Point Point deriving (Show)

-- generating coordinates for leaps
getLeapUpRight :: [String] -> Int -> Int -> Hop
getLeapUpRight board x y =
    let yhop = y-1
        xhop = shift board x y yhop 0
    in
      (Hop (Point xhop yhop) (Point (shift board xhop yhop (yhop-1) 0) (yhop-1)))

getLeapUpLeft :: [String] -> Int -> Int -> Hop
getLeapUpLeft board x y =
    let yhop = y-1
        xhop = shift board x y yhop 1
    in
        (Hop (Point xhop yhop) (Point (shift board xhop yhop (yhop-1) 1) (yhop-1)))

getLeapDownRight :: [String] -> Int -> Int -> Hop
getLeapDownRight board x y =
    let yhop = y+1
        xhop = shift board x y yhop 0
    in
      (Hop (Point xhop yhop) (Point (shift board xhop yhop (yhop+1) 0) (yhop+1)))

getLeapDownLeft :: [String] -> Int -> Int -> Hop
getLeapDownLeft board x y =
    let yhop = y+1
        xhop = shift board x y yhop 1
    in
        (Hop (Point xhop yhop) (Point (shift board xhop yhop (yhop+1) 1) (yhop+1)))

getLeapLeft :: [String] -> Int -> Int -> Hop
getLeapLeft board x y =
    (Hop (Point (x-1) y) (Point (x-2) y))

getLeapRight :: [String] -> Int -> Int -> Hop
getLeapRight board x y =
    (Hop (Point (x+1) y) (Point (x+2) y))

-- returns a possible leap is the coordinates are valid and the peice to jumps is correct
possibleLeap :: [String] -> Hop -> Char -> (Char -> Char) -> [[String]]
possibleLeap board (Hop (Point xhop yhop) (Point xdest ydest)) turn fn =
    if validCoord board xhop yhop && at board xhop yhop == turn
    then
        tryMove board xdest ydest fn
    else
        []

-- generate all possible jumps for x,y for the specified turn
generateLeaps :: [String] -> Int -> Int -> Char -> [[String]]
generateLeaps board x y turn =
    let
        base    = runAction board x y clear
        fn      = setLeap turn
    in
        possibleLeap base (getLeapUpRight base x y)     turn fn ++
        possibleLeap base (getLeapUpLeft base x y)      turn fn ++
        possibleLeap base (getLeapDownRight base x y)   turn fn ++
        possibleLeap base (getLeapDownLeft base x y)    turn fn ++
        possibleLeap base (getLeapRight base x y)       turn fn ++
        possibleLeap base (getLeapLeft base x y)        turn fn

-- generate all possible slides for x,y with the acturn turn
-- Takes a board, an x,y coordinate, the active player, and returns all possible boards
generateSlides :: [String] -> Int -> Int -> Char -> [[String]]
generateSlides board x y turn =
    let
        base    = runAction board x y clear
        fn      = setSlide turn
    in
        tryMove base (x-1) y fn ++
        tryMove base (x+1) y fn ++
        tryMove base (shift board x y (y-1) 0) (y-1) fn ++
        tryMove base (shift board x y (y-1) 1) (y-1) fn ++
        tryMove base (shift board x y (y+1) 0) (y+1) fn ++
        tryMove base (shift board x y (y+1) 1) (y+1) fn

-- generate all possible moves for x,y with the active turn
-- Takes the board, an x,y coordinate and the active player.
-- Returns all possible moves for this player
generatePossible :: [String] -> Int -> Int -> Char -> [[String]]
generatePossible board x y turn =
    generateSlides board x y turn ++
    generateLeaps board x y turn

-- takes parsed boards and prints them nicely
printBoards :: [[String]] -> String
printBoards [] = ""
printBoards (h:t) = (prettyFormat h 3) ++ "\n\n" ++ (printBoards t)
