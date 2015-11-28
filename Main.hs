module Main where

import CrusherWalker as CW
import CrusherEvaluator as CE

-- print (parse "WWW--W--W----BB-BBB" 3)

--main = print(pawnLocations (parse "WWW--W--W----BB-BBB" 3) 'W')
main = print(findBest (parse "WWW--W--W----BB-BBB" 3) 'W' 'B' 3)