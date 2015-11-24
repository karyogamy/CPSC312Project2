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
     
clear c = '-'

setSlide turn c = (if c == '-' then turn else c)

-- perform fn on b @ x,y
runAction b x y fn = runLine y (runLine x fn) b

-- perfom fn on s @ x
runLine x fn s = runLine' x fn s [] where
    runLine' x fn s h
        | x == 0 = h ++ [fn (head s)] ++ (tail s)
        | otherwise =
            runLine' (x-1) fn (tail s) (h ++ [(head s)])

rowLength b y = length (head (drop y b))
            
shift :: [String] -> Int -> Int -> Int -> Int
shift b x y off =
    let currentRl = (rowLength b y)
        otherRl = (rowLength b (y + off))
    in
        if currentRl < otherRl
        then
            (x+1)
        else 
            if currentRl == otherRl
            then
                x
            else
                (x-1)

validCoord b x y =
    (x >= 0) && (y >= 0) && (y < (length b)) && (x < (rowLength b y))
        
onePossible base x y fn =
    let new = runAction base x y fn
    in 
        if (validCoord base x y) && not ((flatten new) == (flatten base)) then
            [new]
        else
            []
            
--jump b xhop yhop xdest ydest turn =
    
          
generatePossible b x y turn = 
    let base = runAction b x y clear
    in
        (onePossible base (x-1) y (setSlide turn)) ++
        (onePossible base (x+1) y (setSlide turn)) ++
        (onePossible base x (y-1) (setSlide turn)) ++
        (onePossible base x (y+1) (setSlide turn)) ++
        (onePossible base (shift b x y (-1)) (y-1) (setSlide turn)) ++
        (onePossible base (shift b x y    1) (y+1) (setSlide turn))

test' [] = ""
test' (h:t) = (prettyFormat h 3) ++ "\n\n" ++ (test' t)
        
test = putStrLn (test' (generatePossible (parse "WWW-WW-------BB-BBB" 3) 1 1 'W'))