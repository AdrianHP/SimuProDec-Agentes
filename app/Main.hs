module Main (main) where

import System.IO
-- import Data.List
import Data
import Environment
import Utils 
import System.Random

main :: IO ()
--Probar movimiento del ninho

main =do
      -- print $ show $ assertProb 50 [1,2,3,4] (mkStdGen 12)
      renderToConsole env
      print $ show env
      renderToConsole env2
      print $ show env2
      renderToConsole env3
      print $ show env3
      renderToConsole env4
      print $ show env4  
      renderToConsole env5
      print $ show env5
     
      
      where env = testEnvironment --generateEnvironment 5 5 3 10 10 50 50
            env2 = moveKid env (2,1) 100 
            env3 = moveKid env2 (1,2) 100
            env4 = moveKid env3 (1,3) 100
            env5 = moveKid env4 (0,4) 100
            -- env3 =fst( moveObstacle env2 (2,1) R )
            -- env4 =fst( moveObstacle env (1,1) L )
            -- env5 =fst( moveObstacle env (1,1) L )
      --   where empty =  emptyEnvironment 5 5
      --         crib =  generateCrib empty 5 5 4
      --         obs =   generateObstacles crib 5 5 3 
      --         dirt = generateDirt obs 5 5 3
      --         kid = generateKids dirt  5 5 4
      --         rob = putRobot kid 5 5
     
-- main = do

--   hSetEcho stdin False
--   hSetBuffering stdin NoBuffering
--   hSetBuffering stdout NoBuffering

--   lvl <- loadLevel "level1.lvl"
--   renderToConsole lvl
--   gameLoop lvl