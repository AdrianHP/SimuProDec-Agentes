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
      renderToConsole env2
      print $ show env2
      renderToConsole env3
      print $ show env3
      renderToConsole env4
      print $ show env4  
      renderToConsole env5
      print $ show env5
     
      
      where env = generateEnvironment 5 5 2 10 10 50 50 3  --testEnvironment
            env2 = kidsActions env
            env3 = kidsActions env2
            env4 = kidsActions env3
            env5 = kidsActions env4
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