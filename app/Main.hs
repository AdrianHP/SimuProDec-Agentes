module Main (main) where

import System.IO
-- import Data.List
import Data
import Environment
import Utils

main :: IO ()
main = print $ show $ generateObstacles env 6 6 3
        where env = generateCrib emptyEnvironment 6 6 4
-- main = print (bfs [(3,3)] [(7,7)] 8)
-- main = putStrLn  $ show $ findNeighbors [(0,0),(5,5)]
-- main = do

--   hSetEcho stdin False
--   hSetBuffering stdin NoBuffering
--   hSetBuffering stdout NoBuffering

--   lvl <- loadLevel "level1.lvl"
--   renderToConsole lvl
--   gameLoop lvl