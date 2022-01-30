{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Cell where

import Data
import Control.Lens.At


cellToStr :: Cell ->String 
cellToStr Empty           = "⬜️"
cellToStr Kid             = "👶🏻"
cellToStr Robot           = "🤖"
cellToStr RobotWithKid    = "⚠️"
cellToStr Dirty           = "💩"
cellToStr Obstacle        = "❌"
cellToStr Crib            = "🟢"
cellToStr FullCrib        = "🔴"

strToCell :: String ->Cell 
strToCell "⬜️" = Empty
strToCell "👶🏻" = Kid         
strToCell "🤖" = Robot       
strToCell "⚠️" = RobotWithKid
strToCell "💩" = Dirty       
strToCell "❌" = Obstacle    
strToCell "🟢" = Crib        
strToCell "🔴" = FullCrib    
    

isEmpty :: Cell -> Bool
isEmpty  Empty = True 
isEmpty _ = False

isRobot :: Cell -> Bool
isRobot  Robot = True 
isRobot _ = False

isKid :: Cell -> Bool
isKid  Kid = True 
isKid _ = False

isRobotWithKid :: Cell -> Bool
isRobotWithKid  RobotWithKid = True 
isRobotWithKid _ = False

isCrib :: Cell -> Bool
isCrib  Crib = True 
isCrib _ = False

isFullCrib :: Cell -> Bool
isFullCrib  FullCrib = True 
isFullCrib _ = False

isDirty :: Cell -> Bool
isDirty  Dirty  = True 
isDirty _ = False

isObstacle :: Cell -> Bool
isObstacle  Obstacle   = True 
isObstacle _ = False