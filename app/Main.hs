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
import System.Posix.Internals (puts)
import Data.Foldable (Foldable(fold))
main :: IO ()
--Probar movimiento del ninho

main =do
      -- putStrLn $ show  result
      -- runTest 20 45 [7,5,3,10,10,5]
      -- runTest 20 2000 [10,10,4,0,10,5]
      runAllTests 20 200 tests
      showSimulation envShow 50 10 10

      where 
            envShow = generateEnvironment 5 5 5 10 10 100 100 3 10 True   0 True    --testEnvironment
            -- envs = generateSimilarEnv [] 4 4 3 10 10 4 False 0 20
            -- result = runList (0,0,0) envs ((length' envs) -1)


showSimulation:: Environment-> Int ->Int -> Int ->IO()
showSimulation env times  d o= do
      putStrLn ("Muestra de simulacion de "++show times++ " pasos ")
      putStrLn ("Tiempo de variacion : "++ show (time env))
      putStrLn ("Filas : " ++show (fst(size env)) ++ "    Columnas : " ++ show (snd(size env)))
      putStrLn ("Bebes : "++show (length' (kids env)))
      putStrLn (" % de suciedad : "++show d ++"   % de obstaculos : " ++show o)
      putStrLn ("Robot modelo "++show (model+1))
      __showSimulation env times 
      where
            t = if (kidPriorityRobot env) then 1 else 0
            model =if  (kidPriorityRobot env) then t+ (modelRobot env) else 0

__showSimulation::Environment -> Int  ->IO()
__showSimulation env 0  =
      let 
         (suc, dirtP) = success env   
      in do 
            putStrLn "Fin de la simulacion"
            putStrLn ( if suc then ("Objetivo cumplido % de suciedad = "++show dirtP) else ("Objetivo fallido % de suciedad = "++show dirtP))
__showSimulation env time  = 
      let 
            env2 = if time>1 then passTime env else env
      in do
            renderToConsole env
            __showSimulation env2 (time-1) 





simulation::Environment -> Int  ->(Bool,Float)
simulation env 0  =
      let 
         (suc, dirtP) = success env   
      in (suc, dirtP)
simulation env time  = 
      let 
            env2 = if time>1 then passTime env else env
      in do
            simulation env2 (time-1) 

runTest::Int->Int->[Int]->IO()
runTest  times turns args =
      let
            envsModel1 = generateSimilarEnv [] (args!!0) (args!!1) (args!!2) (args!!3) (args!!4) (args!!5) False 0 times 
            envsModel2 = generateSimilarEnv [] (args!!0) (args!!1) (args!!2) (args!!3) (args!!4) (args!!5) True  0 times
            envsModel3 = generateSimilarEnv [] (args!!0) (args!!1) (args!!2) (args!!3) (args!!4) (args!!5) True  1 times
            (winsModel1,losesModel1,dirtMean1) = runList (0,0,0) envsModel1 turns (times-1)
            (winsModel2,losesModel2,dirtMean2) = runList (0,0,0) envsModel2 turns (times-1)
            (winsModel3,losesModel3,dirtMean3) = runList (0,0,0) envsModel3 turns (times-1)
      in do 
            putStrLn ""
            putStrLn "Ambiente"
            putStrLn ("Filas : " ++show (args!!0) ++ "    Columnas : " ++ show (args!!1))
            putStrLn ("Bebes : "++show (args!!2))
            putStrLn (" % de suciedad : "++show (args!!3) ++"   % de obstaculos : " ++show (args!!4))
            putStrLn ("Tiempo de variacion : "++ show (args!!5))
            putStrLn ("Simulando en "++show times++ " ambeintes distintos con estas caracteristicas ")
            putStrLn ("Cada simulacion consta de  "++show turns++ " turnos")
            putStrLn "--------------------------------------------------------------------------------  "
            putStrLn "Robot modelo 1 resultados "
            putStrLn ("Exitos: "++ show winsModel1 ++ "   Fallos: "++show losesModel1 ++"  Porciento medio de casillas sucias: "++ show  (dirtMean1/ fromInteger (toInteger times)  )  )
            putStrLn "  "
            putStrLn "Robot modelo 2 resultados "
            putStrLn ("Exitos: "++ show winsModel2 ++ "   Fallos: "++show losesModel2 ++"  Porciento medio de casillas sucias: "++ show  (dirtMean2/ fromInteger (toInteger times)  )  )
            putStrLn "  "
            putStrLn "Robot modelo 3 resultados "
            putStrLn ("Exitos: "++ show winsModel3 ++ "   Fallos: "++show losesModel3 ++"  Porciento medio de casillas sucias: "++ show  (dirtMean3/ fromInteger (toInteger times)  )  )
            putStrLn " "
            putStrLn "#################################################################################### "

runAllTests::Int->Int->[[Int]]  -> IO()
runAllTests _ _ []  = print "Tests finalizados"
runAllTests times tunrs (test:rest) = 
        do
            runTest times tunrs test
            runAllTests times tunrs rest      
 

runList::(Int,Int,Float) -> [Environment ] -> Int -> Int -> (Int,Int,Float)
runList result _ _ (-1) =result
runList result envs turns count =
      let
         currentEnv = envs!!count
         (suc,dirtPercent) = simulation currentEnv turns
         (w,l,p)  = result
         var = if suc then(w+1,l,p+dirtPercent) else (w,l+1,p+dirtPercent)
      in runList var envs turns (count-1)


generateSimilarEnv::[Environment ] -> Int -> Int -> Int -> Int -> Int ->  Int -> Bool -> Int-> Int-> [Environment]
generateSimilarEnv result _ _ _ _ _ _  _ _ 0 = result
generateSimilarEnv result m n kids obstaclePercent dirtPercent t  kidPrior model count =  
      let
            env = generateEnvironment m n kids obstaclePercent dirtPercent 50 50 t count kidPrior model False
      in generateSimilarEnv (env:result) m n kids obstaclePercent dirtPercent t  kidPrior model (count-1)

tests::[[Int]]
tests = [[4,4,3,10,10,4],
         [5,5,4,10,10,4],
         [5,5,5,10,10,5],
         [6,6,7,10,20,5],
         [7,10,10,10,10,5],
         [8,8,15,15,15,5],
         [10,8,12,10,10,5],
         [10,10,18,20,20,5],
         [10,10,15,10,10,5]] 