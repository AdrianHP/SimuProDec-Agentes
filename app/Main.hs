module Main (main) where

import System.IO
-- import Data.List
import Data
import Environment
import Utils 
import System.Random
import Kid
import Utils
import Control.Applicative (Alternative(empty))
import Environment (makePath, closestObjects)
main :: IO ()
--Probar movimiento del ninho

main =do
      simulate env 35 3
--      print $show bf
--      print $show p
      -- renderToConsole env
      -- renderToConsole env2
      -- renderToConsole env3
      -- renderToConsole env4
      -- renderToConsole env5
      -- renderToConsole env6
      -- -- print $ show env7
      -- renderToConsole env7
      -- renderToConsole env8
      -- renderToConsole env9
      -- renderToConsole env10
     
      
      where env = generateEnvironment 5 5 3 10 10 100 100 3 10  --testEnvironment
      --       env2 = chooseAction env True 
      --       env3 = chooseAction env2 True 
      --       env4 = chooseAction env3 True 
      --       env5 = chooseAction env4 True 
      --       env6 = chooseAction env5 True 
      --       env7 = chooseAction env6 True 
      --       env8 = chooseAction env7 True 
      --       env9 = chooseAction env8 True 
      --       env10 = chooseAction env9 True 
            -- l = emptyList 10
            -- (bf,p) = bfsKid env [(0,0)] [] [(5,5)] (emptyList 25) 25 
            -- path = makePath [] p (4,2) 5
            -- bf = closestObjects env (0,0)

simulate::Environment -> Int -> Int ->IO()
simulate env 0 _ = print "End Simulation"
simulate env time t = do
      renderToConsole env
      simulate env2 (time-1) t
      where 
            env2 = chooseAction env True