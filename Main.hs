module Main where

import CrusherWalker as CW
import CrusherEvaluator as CE

-- print (parse "WWW--W--W----BB-BBB" 3)

main = putStrLn(test' (possibleCrushes (parse "WWW--W--W----BB-BBB" 3) 'W' 'B'))