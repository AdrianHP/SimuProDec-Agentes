module Main (main) where

import System.IO
-- import Data.List
import Data
import Environment
import Utils 
import System.Random
import Kid
main :: IO ()
--Probar movimiento del ninho

main =do
      -- print $ show  k2
      renderToConsole env
      renderToConsole env2
      renderToConsole env3
      renderToConsole env4
      renderToConsole env5
      renderToConsole env6
      print $ show env7
      renderToConsole env7
      renderToConsole env8
      renderToConsole env9
      renderToConsole env10
     
      
      where env = generateEnvironment 5 5 2 10 10 50 50 3 10  --testEnvironment
            env2 = moveRobot env (0,1) (1,1)
            env3 = clean env2  (1,1)
            env4 = moveRobot env3 (1,1) (1,2)
            env5 = moveRobot env4 (1,2) (1,3)
            env6 = moveRobot env5 (1,3) (2,3)
            env7 = moveAndTake env6 (2,3) (3,3)
            env8 = moveRobot env7 (3,3) (3,4)
            env9 = Environment.drop env8 (3,4) 
            env10 = moveRobot env9 (3,4) (3,3)
            -- kid = Kid (0,0) False False
            -- kids = [kid,kid,kid]
            -- k2 = map position kids


-- main = do

--   hSetEcho stdin False
--   hSetBuffering stdin NoBuffering
--   hSetBuffering stdout NoBuffering

--   lvl <- loadLevel "level1.lvl"
--   renderToConsole lvl
--   gameLoop lvl