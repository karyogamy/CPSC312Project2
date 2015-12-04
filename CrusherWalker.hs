module CrusherWalker where

getTakeNums n =
    let n2temp = [ n2 | n2 <- [n,n+1..(2*n-2)]]
    in n2temp ++ [2*n-1] ++ (reverse n2temp)
-- "WWW­WW­­­­­­­BB­BBB"
parse s n = reverse (parse' s (getTakeNums n) 0 []) where
    parse' s [] skip ret = ret
    parse' s (h:t) skip ret = parse' s t (skip + h) ((take h (drop skip s)) : ret)

spaceOut s = spaceOut' s "" where
    spaceOut' [] ret = ret
    spaceOut' (ch:t) ret = spaceOut' t (ret ++ [ch, ' '])

prettyFormat :: [String] -> Int -> String
prettyFormat b n
    | null b = ""
    | otherwise =
        let h = (head b)
        in [ ' ' | _ <- [0, 1.. (2*n-1-(length h))]] ++ (spaceOut h) ++ "\n" ++ (prettyFormat (tail b) n)

flatten (h:t) = h ++ (flatten t)
flatten [] = []

-- callbacks for runAction
clear c = '-'
setSlide turn c = (if c == '-' then turn else c)
setLeap turn c = if not (c == turn) then turn else c

-- perform fn on b @ x,y
runAction b x y fn = runLine y (runLine x fn) b

-- perfom fn on s @ x
runLine x fn s = runLine' x fn s [] where
    runLine' x fn s h
        | x == 0 = h ++ [fn (head s)] ++ (tail s)
        | otherwise =
            runLine' (x-1) fn (tail s) (h ++ [(head s)])

at board x y = head (drop x (head (drop y board)))

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

validCoord board x y =
    (x >= 0) && (y >= 0) && (y < (length board)) && (x < (rowLength board y))

-- try move pawn to new x and y by applying fn (setSlide or setLeap)
-- if succeed return new board
-- if fail return empty
tryMove base x y fn =
    let new = runAction base x y fn
    in
        if (validCoord base x y) && not ((flatten new) == (flatten base)) then
            [new]
        else
            []

data Point = Point Int Int deriving (Show)
data Hop = Hop Point Point deriving (Show)

-- generating coordinates for Leaps
getLeapUpRight board x y =
    let yhop = y-1
        xhop = shift board x y yhop 0
    in
      (Hop (Point xhop yhop) (Point (shift board xhop yhop (yhop-1) 0) (yhop-1)))

getLeapUpLeft board x y =
    let yhop = y-1
        xhop = shift board x y yhop 1
    in
        (Hop (Point xhop yhop) (Point (shift board xhop yhop (yhop-1) 1) (yhop-1)))

getLeapDownRight board x y =
    let yhop = y+1
        xhop = shift board x y yhop 0
    in
      (Hop (Point xhop yhop) (Point (shift board xhop yhop (yhop+1) 0) (yhop+1)))

getLeapDownLeft board x y =
    let yhop = y+1
        xhop = shift board x y yhop 1
    in
        (Hop (Point xhop yhop) (Point (shift board xhop yhop (yhop+1) 1) (yhop+1)))

getLeapLeft board x y =
    (Hop (Point (x-1) y) (Point (x-2) y))

getLeapRight board x y =
    (Hop (Point (x+1) y) (Point (x+2) y))

possibleLeap board (Hop (Point xhop yhop) (Point xdest ydest)) turn fn =
    --if (validCoord board xhop yhop) && (not ((at board xhop yhop) == '-'))
    if validCoord board xhop yhop && at board xhop yhop == turn
    then
        tryMove board xdest ydest fn
    else
        []

-- generate all possible leaps for x,y with the turn function
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

-- generate all possible slides for x,y with the acturn turn
generatePossibleMoves board x y turn =
    generateSlides board x y turn ++
    generateLeaps board x y turn


test' [] = ""
test' (h:t) = (prettyFormat h 3) ++ "\n\n" ++ (test' t)

test = putStrLn (test' (generatePossibleMoves (parse "WWW-WW-------BB-BBB" 3) 1 1 'W'))
test2 = putStrLn (test' (generatePossibleMoves (parse "WWW--W--W----BB-BBB" 3) 1 3 'B'))
