module Data where
import System.Random

type Coord = (Int, Int)








data Direction = Up 
             | Down 
             | L 
             | R 
             | UpLeft 
             | UpRight
             | DownLeft
             | DownRight
             | UpUp 
             | DownDown
             | LeftLeft 
             | RightRight deriving (Show)