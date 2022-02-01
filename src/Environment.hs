module Environment where
import Data 

import Control.Monad (guard)
import Debug.Trace (trace)
import Utils
import System.Random

-- import Utils




generateEnvironment :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Environment
generateEnvironment m n kids obstaclesPercentage dirtPercentage dirtProb moveKidProb =
    let 
        totalObst = div (obstaclesPercentage * m * n)  100
        totalDirt = div  ( dirtPercentage * m * n )  100
        empty = emptyEnvironment m n dirtProb moveKidProb
        crib = generateCrib empty m n kids
        obs = generateObstacles crib m n totalObst
        dirt = generateDirt obs m n totalDirt
        kid = generateKids dirt  m n kids
        env = putRobot kid m n
    in env


testEnvironment ::  Environment 
testEnvironment  = Environment { 
    size = (4,5)
    , robot = (0,0)
    , robotWithKid = (0,0)
    , kids = [(2,1),(2,3)]
    , crib = []
    , fullCrib = []
    , dirty = []
    , obstacle = [(1,2)]
    , empty =  [(0,1),(1,1),(0,2),(0,3),(0,4),(1,0),(1,4),(2,0),(1,3),(2,2),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4)]
    ,isHoldingKid = False
    , dirtProb = 100
    , moveKidProb = 10
    , remaininKids = 1
    , randomGen =mkStdGen 10
    }


emptyEnvironment ::Int -> Int -> Int -> Int -> Environment 
emptyEnvironment m n dirtProb moveKidProb  = Environment { 
    size = (m-1,n-1)
    , robot = (0,0)
    , robotWithKid = (0,0)
    , kids = []
    , crib = []
    , fullCrib = []
    , dirty = []
    , obstacle = []
    , empty = initEmptys m n n []
    ,isHoldingKid = False
    , dirtProb = dirtProb
    , moveKidProb = moveKidProb
    , remaininKids = 0
    , randomGen =mkStdGen 10
    }

initEmptys :: Int -> Int -> Int -> [Coord] ->[Coord]
initEmptys 0 0 n2 result = (0,0):result
initEmptys m n n2 result 
                    | n==0 = initEmptys (m-1) n2 n2 ((m,n):result)
                    | otherwise = initEmptys m (n-1) n2 ((m,n):result)


putRobot::Environment -> Int -> Int  -> Environment 
putRobot env m n =
    let   
        (x,gen) = myRandom 0 n (randomGen env) 
        (y,gen2) = myRandom 0 m gen
    in if (x,y) `elem` empty  env 
        then 
            let emptyNew = remove (empty env) (x,y) 
                envNew = env{empty = emptyNew}  
            in env{robot=(x,y) , empty =  emptyNew ,randomGen = gen2}
        else putRobot env m n 

generateCrib :: Environment -> Int -> Int ->Int  -> Environment
generateCrib env  m n kids  = 
    let count = kids
        (srcX,gen) = myRandom 0 n (randomGen env) 
        (srcY,gen2) = myRandom 0 m gen
        c = bfs [(srcX,srcY)] [(m,n)] kids
        emptyOld = empty env
        emptyNew = filter (`notElem` c) emptyOld
    in env{crib  = c , empty =  emptyNew ,randomGen = gen2} 

generateObstacles :: Environment ->Int -> Int -> Int -> Environment 
generateObstacles =generateObstacles2 []

generateObstacles2::[Coord] -> Environment ->Int -> Int -> Int -> Environment 
generateObstacles2 obsL env m n 0  = 
    let 
        emptyOld = empty env
        emptyNew = filter (`notElem` obsL) emptyOld
    in env{obstacle=obsL , empty =  emptyNew}
generateObstacles2 obsL env m n obs = 
    let count = obs   
        (x,gen) = myRandom 0 n (randomGen env) 
        (y,gen2) = myRandom 0 m gen
    in if (x,y) `elem` empty  env  
        then 
            let emptyNew = remove (empty env) (x,y) 
                envNew = env{empty = emptyNew  ,randomGen = gen2}  
            in generateObstacles2 ((x,y):obsL) envNew m n (obs-1) 
        else  generateObstacles2 obsL env m n obs

generateKids :: Environment ->Int -> Int -> Int -> Environment 
generateKids =generateKids2 []

generateKids2::[Coord] -> Environment ->Int -> Int -> Int -> Environment 
generateKids2 obsL env m n 0  = 
    let 
        emptyOld = empty env
        emptyNew = filter (`notElem` obsL) emptyOld
    in env{ kids=obsL , empty =  emptyNew, remaininKids = length' obsL }
generateKids2 obsL env m n obs = 
    let count = obs   
        (x,gen) = myRandom 0 n (randomGen env) 
        (y,gen2) = myRandom 0 m gen
    in if (x,y) `elem` empty  env  
        then 
            let emptyNew = remove (empty env) (x,y) 
                envNew = env{empty = emptyNew ,randomGen = gen2}  
            in generateKids2 ((x,y):obsL) envNew m n (obs-1) 
        else  generateKids2 obsL env m n obs

generateDirt :: Environment ->Int -> Int -> Int -> Environment 
generateDirt =generateDirt2 []

generateDirt2::[Coord] -> Environment ->Int -> Int -> Int -> Environment 
generateDirt2 obsL env m n 0  = 
    let 
        emptyOld = empty env
        emptyNew = filter (`notElem` obsL) emptyOld
    in env{dirty=obsL , empty =  emptyNew}
generateDirt2 obsL env m n obs = 
    let count = obs   
        (x,gen) = myRandom 0 n (randomGen env) 
        (y,gen2) = myRandom 0 m gen
    in if (x,y) `elem` empty  env  
        then 
            let emptyNew = remove (empty env) (x,y) 
                envNew = env{empty = emptyNew,randomGen = gen2}  
            in generateDirt2 ((x,y):obsL) envNew m n (obs-1) 
        else  generateDirt2 obsL env m n obs



getTile:: Environment -> Coord -> Char
getTile env c 
    | isRobot         env c = '*'--'🤖'
    | isRobotWithKid  env c = '+'--'⚠️' 
    | isKid           env c = '@'--'👶🏻'
    | isCrib          env c = 'o'--'🟢'
    | isFullCrib      env c = 'O'--'🔴'
    | isDirty         env c = '#'--'💩'
    | isObstacle      env c = 'X'--'❌'
    | isEmpty         env c = '_'--'⬜️'

showEnvironment :: Environment -> String
showEnvironment env = unlines tileLines
  where
    (sizeX, sizeY) = size env
    tileLines = [[getTile env (y, x) | x <- [0..sizeY-1]]
                                     | y <- [0..sizeX-1]]

renderToConsole :: Environment -> IO()
renderToConsole =
  putStrLn . showEnvironment




moveKid::Environment -> Coord -> Int -> Environment
moveKid env (x,y) dirtProb = 
    if isInside newPos m n then  
        if isObstacle env newPos then 
            let 
              (env2,canMove) = moveObstacle env newPos newDir  
              emptyOld = empty env2
              emptyNew = (x,y):(remove emptyOld newPos)
              kidsOld = kids env2
              kidsNew = (newPos):(remove kidsOld (x,y))
              envIfCanMov = env2{kids =kidsNew , empty = emptyNew}
              envIfCanMovPutDirt = putDirt envIfCanMov (x,y) newPos dirtProb 
              envPutDirt = putDirt env (x,y) (x,y) dirtProb 
            in if canMove then trace("Obstacle and move ") envIfCanMovPutDirt{randomGen = gen} else trace("Obstacle no Move") env{randomGen = gen}
        else if isEmpty env newPos then
            let
              emptyOld = empty env
              emptyNew = (x,y):(remove emptyOld newPos)
              kidsOld = kids env
              kidsNew = (newPos):(remove kidsOld (x,y))
              env2 = env{kids =kidsNew , empty = emptyNew}
              envPutDirt = putDirt env2 (x,y) newPos dirtProb 
            in  trace("Emty and move ") envPutDirt{randomGen = gen}
            else trace("Not Emty  ")  env{randomGen = gen}
    else trace("else ")  env{randomGen = gen}
    
    
    where (rnd,gen) =  myRandom 0 8 (randomGen env )
          newDir =  directions !! rnd
          newPos = move (x,y) newDir
          m = fst (size env) 
          n = snd (size env)
          

moveObstacle:: Environment -> Coord -> Moves ->(Environment,Bool )
moveObstacle env coord dir 
    | isInside newPos m n && isEmpty env newPos = 
        let 
            obstacles = newPos:remove  (obstacle env) coord
            emptys = coord:remove (empty env) newPos 
            env2 = env { obstacle = obstacles, empty = emptys}
            
        in trace ("Here " ++ show newPos) (env2,True )
    | isInside newPos m n && isObstacle env newPos &&  snd (moveObstacle env newPos dir)  =
        let 
            env1= fst (moveObstacle env newPos dir)
            obstacles = newPos:remove  (obstacle env1) coord
            emptys = coord:remove (empty env1) newPos 
            env2 = env { obstacle = obstacles, empty = emptys}
            
        in  trace ("Here2 " ++ show newPos) (env2,True )
    | otherwise =trace ("Here3 " ++ show newPos) (env,False )
    where 
        newPos =  move coord dir
        m = fst (size env) 
        n = snd (size env)

putDirt ::Environment -> Coord -> Coord -> Int -> Environment
putDirt env (x,y) (posKidX,posKidy) dirtProb  = 
    let 
        (m,n) =  size env 
        neighbors = findNeighbors8 (x,y) (m,n)
        kids = length' (remove (filter (isKid env) neighbors) (posKidX,posKidy))
        freeCells =(x,y): filter (isEmpty env) neighbors
        freeCellsCount = length' freeCells
        dirtTargets 
            | kids == 0 = 
                if freeCellsCount > 0 then
                    let
                        (rnd,gen) = myRandom 0 (length' freeCells) (randomGen env)
                    in  [freeCells!!rnd]
                else []
            | kids == 1 = 
                 if freeCellsCount < 3 then
                    freeCells
                else 
                    let
                        (rnd,gen2) = myRandom 0 (length' freeCells) (randomGen env )
                        (rnd1,gen3) = myRandom 0 ((length' freeCells)-1) gen2
                        (rnd2,gen4) = myRandom 0 ((length' freeCells)-2) gen3
                        cell = freeCells!!rnd
                        freeCells1 = remove freeCells cell
                        cell1 = freeCells1!!rnd1
                        freeCells2 = remove freeCells1 cell1
                        cell2 = freeCells2!!rnd2
                    in [cell,cell1,cell2]
            | otherwise = freeCells
                

                    
                   
        dirtyCells =(dirty env)++  assertProb dirtProb dirtTargets (randomGen env)
        emptyOld = empty env
        emptyNew = filter (`notElem` dirtyCells) emptyOld
    in env{dirty = dirtyCells,empty = emptyNew }
      



isRobot :: Environment -> Coord  -> Bool
isRobot env coord = robot env ==coord && not (isHoldingKid env)

isRobotWithKid :: Environment -> Coord  -> Bool
isRobotWithKid  env coord = robotWithKid env ==coord && isHoldingKid env


isEmpty :: Environment -> Coord  -> Bool
isEmpty env coord = coord `elem` empty env


isKid :: Environment -> Coord  -> Bool
isKid  env coord= coord `elem` kids env 

isCrib :: Environment -> Coord  -> Bool
isCrib  env coord= coord `elem` crib  env 

isFullCrib :: Environment -> Coord  -> Bool
isFullCrib  env coord= coord `elem` fullCrib  env 

isDirty ::Environment -> Coord  -> Bool
isDirty  env coord= coord `elem` dirty  env 


isObstacle :: Environment -> Coord  -> Bool
isObstacle  env coord= coord `elem` obstacle  env 

