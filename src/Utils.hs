
module Utils where
import Control.Arrow (Arrow(first))
import Data

import System.IO.Unsafe  -- be careful!                                          
import System.Random 
import Control.Exception (assert)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

first' :: [a]->a
first' (x:xs) = x

remove :: (Eq a) => [a] -> a -> [a]
remove l n = [ x | x <- l , x /= n]


initRandom = mkStdGen

myRandom :: Int -> Int -> StdGen  ->(Int,StdGen) 
myRandom min max  = randomR (min, (max-1)) 


directions = [Up,Down,L,R,UpLeft ,UpRight,DownLeft,DownRight]

move :: Coord -> Direction -> Coord 
move (x,y) Up         = (x-1,y)
move (x,y) Down       = (x+1,y)
move (x,y) R          = (x,y+1) 
move (x,y) L          = (x,y-1)
move (x,y) UpLeft     = (x-1,y-1)
move (x,y) UpRight    = (x-1,y+1)
move (x,y) DownLeft   = (x+1,y-1)
move (x,y) DownRight  = (x+1,y+1)
move (x,y) UpUp       = (x-2,y)
move (x,y) DownDown   = (x+2,y)
move (x,y) LeftLeft   = (x,y-2)
move (x,y) RightRight = (x,y+2)

bfs:: [Coord] ->[Coord ]  -> Int -> [Coord ]
bfs (queue:r) limits  count = bfs2 (queue:r) r limits count
                    
bfs2::[Coord ] -> [Coord ] -> [Coord ] ->Int ->[Coord]
bfs2 [] visit limits count = visit
bfs2 q visit _ 0 = visit

bfs2  (q:rest) visit limits count
                            | q `elem` visit = bfs2 rest visit limits count
                            | otherwise = bfs2 neighbors v limits (count-1)
                            where  neighbors = rest ++ findNeighbors  q limits
                                   v = q:visit


-- bfs:: [Coord] ->[Coord ]  -> Int -> [Coord ]
-- bfs (queue:r) limits  count = bfs2 (queue:r) r limits count
                    



emptyList:: Int ->[Coord]
emptyList n = [(-1,-1) | x<-[1..n]]


findNeighbors :: Coord ->[Coord]  -> [Coord]
findNeighbors coord  ((m,n):rest) = filter checkInside  (rest ++ [(move coord Up) ,(move coord  Down),(move coord L),(move coord  R) ] )
                                        where checkInside coord2 = isInside coord2 m n

findNeighbors8 :: Coord -> Coord  -> [Coord]
findNeighbors8 coord  (m,n) = filter checkInside  (map (move coord) (directions) )
                                        where checkInside coord2 = isInside coord2 m n

isInside :: Coord -> Int -> Int -> Bool
isInside (x,y) m n = x>=0 && x <m && y>=0 &&y<n  


assertProb::Int -> [a] -> StdGen -> ([a],StdGen)
assertProb _ [] gen = ([],gen)
assertProb prob (c:rest) gen = 
       let 
          (var,gen2) = myRandom 0 100 gen
       in if prob > var then 
              let
                  (list,gen) =   assertProb prob rest gen2
                  result = (c:list,gen)
              in result
          else  assertProb prob rest gen2

updatePath::[Coord] ->[Coord]->[Int]->Coord -> Int->[Coord]
updatePath result _ _ _ (-1) = result
updatePath result (p:path) nIndex c len =
    if len `elem` nIndex && ((p:path)!!len) ==(-1,-1) then
        updatePath (c:result) (p:path) nIndex c (len-1)
    else
        updatePath (((p:path))!!len:result) (p:path) nIndex c (len-1)


makePath:: [Coord] -> [Coord] -> Coord ->Int -> [Coord]
makePath  result path (x,y) n = if path!!index==(-1,-1) then result else makePath (newCoord:result) path newCoord n
                         where
                             index = x*n+y
                             newCoord = path!!index





data Queue a = Queue [a] deriving (Show)

push :: a -> Queue a -> Queue a
push e (Queue es) = Queue (es ++ [e])

pop :: Queue a -> (a, Queue a)
pop (Queue xs) = (head xs, Queue $ tail xs)
