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
      print $ show env
      -- renderToConsole env2
      -- print $ show env2
      -- renderToConsole env3
      -- print $ show env3
      -- renderToConsole env4
      -- print $ show env4  
      -- renderToConsole env5
      -- print $ show env5
     
      
      where env = generateEnvironment 5 5 2 10 10 50 50  --testEnvironment
            -- env2 = moveKid env (2,1) 100 
            -- env3 = moveKid env2 (1,2) 100
            -- env4 = moveKid env3 (1,3) 100
            -- env5 = moveKid env4 (0,4) 100
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