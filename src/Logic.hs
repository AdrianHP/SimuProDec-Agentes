module Logic where


import System.IO

import Data 







-- isEmpty :: Environment -> Coord  -> Bool
-- isEmpty lvl c = c `elem` 
-- isEmpty _ = False

isRobot :: Environment -> Coord  -> Bool
isRobot env coord = robot env ==coord

isRobotWithKid :: Environment -> Coord  -> Bool
isRobotWithKid  env coord = robotWithKid env ==coord

isKid :: Environment -> Coord  -> Bool
isKid  env coord= coord `elem` kids env 

isCrib :: Environment -> Coord  -> Bool
isCrib  env coord= coord `elem` crib  env 

isFullCrib :: Environment -> Coord  -> Bool
isFullCrib  env coord= coord `elem` fullCrib  env 

isDirty ::Environment -> Coord  -> Bool
isDirty  env coord= coord `elem` dirty  env 


isObstacle :: Environment -> Coord  -> Bool
isObstacle  env coord= coord `elem` obstacle  env 

isEmpty :: Environment -> Coord  -> Bool
isEmpty  env coord= coord `elem` empty  env 

