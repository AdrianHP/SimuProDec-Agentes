{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Utils where
import Control.Arrow (Arrow(first))
import Data

import System.IO.Unsafe  -- be careful!                                          
import System.Random 

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

first' :: [a]->a
first' (x:xs) = x


myRandom :: Int -> Int ->Int 
myRandom min max = unsafePerformIO (getStdRandom (randomR (min, max)))




move :: Coord -> Moves -> Coord 
move (x,y) L          = (x-1,y)
move (x,y) R          = (x+1,y)
move (x,y) Up         = (x,y-1) 
move (x,y) Down       = (x,y+1)
move (x,y) UpLeft     = (x-1,y-1)
move (x,y) UpRight    = (x+1,y-1)
move (x,y) DownLeft   = (x-1,y+1)
move (x,y) DownRight  = (x+1,y+1)
move (x,y) UpUp       = (x,y-2)
move (x,y) DownDown   = (x,y+2)
move (x,y) LeftLeft   = (x-2,y)
move (x,y) RightRight = (x+2,y)



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


findNeighbors :: Coord ->[Coord]  -> [Coord]
findNeighbors coord  ((m,n):rest) = filter checkInside  (rest ++ [(move coord Up) ,(move coord  Down),(move coord L),(move coord  R) ] )
                                        where checkInside coord2 = isInside coord2 m n


isInside :: Coord -> Int -> Int -> Bool
isInside (x,y) m n = x>=0 && x <m && y>=0 &&y<n  



data Queue a = Queue [a] deriving (Show)

push :: a -> Queue a -> Queue a
push e (Queue es) = Queue (es ++ [e])

pop :: Queue a -> (a, Queue a)
pop (Queue xs) = (head xs, Queue $ tail xs)
