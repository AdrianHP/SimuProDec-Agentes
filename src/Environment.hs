module Environment where
import Data 

import Control.Monad (guard)
import Debug.Trace (trace)
import Utils
import System.Random
import Kid
import Robot


---Environment structure

data Environment = Environment {
                    size    :: Coord
                ,   robot  :: Robot
                ,   kids   :: [Kid]
                ,   crib  :: [Coord]
                ,   fullCrib :: [Coord]
                ,   dirty :: [Coord]
                ,   obstacle :: [Coord]
                ,   empty ::[Coord]
                ,   remaininKids :: Int 
                ,   moveKidProb :: Int
                ,   dirtProb :: Int
                ,  randomGen :: StdGen
                ,  time ::Int
                ,  turn ::Int
                } deriving (Eq ,Show)

-------------------------------------------


-- Generator-------------------------------


testEnvironment ::  Environment 
testEnvironment  = Environment { 
    size = (4,5)
    , robot = Robot{pos = (0,0), isHoldingKid = False}
    , kids = [initKid (2,1),initKid (2,3)]
    , crib = []
    , fullCrib = []
    , dirty = []
    , obstacle = [(1,2)]
    , empty =  [(0,1),(1,1),(0,2),(0,3),(0,4),(1,0),(1,4),(2,0),(1,3),(2,2),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4)]
    , dirtProb = 100
    , moveKidProb = 10
    , remaininKids = 1
    , randomGen =mkStdGen 10
    , time = 0
    , turn = 0
    }


generateEnvironment :: Int -> Int -> Int -> Int -> Int -> Int -> Int ->Int -> Int -> Environment
generateEnvironment m n kids obstaclesPercentage dirtPercentage dirtProb moveKidProb t rndSeed =
    let 
        totalObst = div (obstaclesPercentage * m * n)  100
        totalDirt = div  ( dirtPercentage * m * n )  100
        empty = emptyEnvironment m n dirtProb moveKidProb t rndSeed
        crib = generateCrib empty m n kids
        obs =   generateObstacles crib totalObst
        dirt = generateDirt obs  totalDirt
        kid = generateKids dirt  kids
        env = putRobot kid 
    in  env

emptyEnvironment ::Int -> Int -> Int -> Int -> Int -> Int -> Environment 
emptyEnvironment m n dirtProb moveKidProb t rndSeed = Environment { 
    size = (m,n)
    , robot = Robot{pos = (0,0), isHoldingKid = False}
    , kids = []
    , crib = []
    , fullCrib = []
    , dirty = []
    , obstacle = []
    , empty = initEmptys (m-1) (n-1)  (n-1) []
    , dirtProb = dirtProb
    , moveKidProb = moveKidProb
    , remaininKids = 0
    , randomGen =mkStdGen rndSeed
    , time = t
    , turn = 0
    }

initEmptys :: Int -> Int -> Int -> [Coord] ->[Coord]
initEmptys 0 0 n2 result = (0,0):result
initEmptys m n n2 result 
                    | n==0 = initEmptys (m-1) n2 n2 ((m,n):result)
                    | otherwise = initEmptys m (n-1) n2 ((m,n):result)

putRobot::Environment  -> Environment 
putRobot env  =
     let 
        emptyOld = empty env
        (rnd,gen) = myRandom 0 (length' emptyOld) (randomGen env) 
        elem = (emptyOld)!!rnd
        emptyNew = remove emptyOld elem
        itemNew = Robot elem False
        env2 = env{empty =emptyNew , robot = itemNew , randomGen = gen} 
      
    in env2

generateCrib :: Environment -> Int -> Int ->Int  -> Environment
generateCrib env  m n kids  = 
    let count = kids
        (srcX,gen) = myRandom 0 n (randomGen env) 
        (srcY,gen2) = myRandom 0 m gen
        c = bfs [(srcX,srcY)] [(m,n)] kids
        emptyOld = empty env
        emptyNew = filter (`notElem` c) emptyOld
    in env{crib  = c , empty =  emptyNew ,randomGen = gen2} 

generateObstacles:: Environment -> Int -> Environment 
generateObstacles env 0  = env
generateObstacles env items = 
    let 
        emptyOld = empty env
        (rnd,gen) = myRandom 0 (length' emptyOld) (randomGen env) 
        elem = (emptyOld)!!rnd
        emptyNew = remove emptyOld elem
        itemNew = elem:obstacle env 
        env2 = env{empty =emptyNew , obstacle = itemNew , randomGen = gen} 
      
    in generateObstacles env2  (items-1)

generateKids:: Environment -> Int -> Environment 
generateKids env 0  = env
generateKids env items = 
    let 
        emptyOld = empty env
        (rnd,gen) = myRandom 0 (length' emptyOld) (randomGen env) 
        elem = (emptyOld)!!rnd
        emptyNew = remove emptyOld elem
        itemNew = (initKid elem): kids env 
        env2 = env{empty =emptyNew , kids = itemNew , randomGen = gen} 
      
    in generateKids env2  (items-1)

generateDirt:: Environment -> Int -> Environment 
generateDirt env 0  = env
generateDirt env items = 
    let 
        emptyOld = empty env
        (rnd,gen) = myRandom 0 (length' emptyOld) (randomGen env) 
        elem = (emptyOld)!!rnd
        emptyNew = remove emptyOld elem
        itemNew = elem:dirty env 
        env2 = env{empty =emptyNew , dirty = itemNew , randomGen = gen} 
      
    in generateDirt env2  (items-1)

--------------------------------------------------




---Variations

-- passTime::Environment -> Environment
-- passTime = variateEnvironment $ variateAgents env

variateEnvironment::Environment -> Environment
variateEnvironment env = env

agentsActions::Environment -> Environment
agentsActions env = env

--------------------------------

-----Kids Actions----------------

kidsActions::Environment -> Environment
kidsActions env =
    let 
        moveProb = moveKidProb env
        (temp,gen) = assertProb moveProb (kids env) (randomGen env)
        kidsToMove = map position temp 
        envAfterMove = moveAllKids env kidsToMove
        i =  trace("kids to move" ++show kidsToMove ++ show envAfterMove)
    in envAfterMove

moveAllKids::Environment -> [Coord] -> Environment
moveAllKids env [] = env
moveAllKids env (k:kids) = 
    let 
        env2 = moveKid env k (dirtProb env)
    in moveAllKids env2 kids  


moveKid::Environment -> Coord -> Int -> Environment
moveKid env (x,y) dirtProb = 
    if isInside newPos m n then  
        if isObstacle env newPos then 
            let 
              (env2,canMove) = moveObstacle env newPos newDir  
              emptyOld = empty env2
              emptyNew = (x,y):(remove emptyOld newPos)
              kidsOld = kids env2
              kidsNew = (initKid newPos):(remove kidsOld (initKid (x,y)))
              envIfCanMov = env2{kids =kidsNew , empty = emptyNew}
              envIfCanMovPutDirt = putDirt envIfCanMov (x,y) newPos dirtProb 
              envPutDirt = putDirt env (x,y) (x,y) dirtProb 
            in if canMove then trace("Obstacle and move ") envIfCanMovPutDirt{randomGen = gen} else trace("Obstacle no Move") env{randomGen = gen}
        else if isEmpty env newPos then
            let
              emptyOld = empty env
              emptyNew = (x,y):(remove emptyOld newPos)
              kidsOld = kids env
              kidsNew = (initKid newPos):(remove kidsOld (initKid (x,y)))
              env2 = env{kids =kidsNew , empty = emptyNew}
              envPutDirt = putDirt env2 (x,y) newPos dirtProb 
            in  trace("Emty and move ") envPutDirt{randomGen = gen}
            else trace("Not Emty  ")  env{randomGen = gen}
    else trace("else ")  env{randomGen = gen}
    
    
    where (rnd,gen) =  myRandom 0 4 (randomGen env )
          newDir =  directions !! rnd
          newPos = move (x,y) newDir
          m = fst (size env) 
          n = snd (size env)
          

moveObstacle:: Environment -> Coord -> Direction ->(Environment,Bool )
moveObstacle env coord dir 
    | isInside newPos m n && isEmpty env newPos = 
        let 
            obstacles = newPos:remove  (obstacle env) coord
            emptys = coord:remove (empty env) newPos 
            env2 = env { obstacle = obstacles, empty = emptys}
            
        in  (env2,True )
    | isInside newPos m n && isObstacle env newPos &&  snd (moveObstacle env newPos dir)  =
        let 
            env1= fst (moveObstacle env newPos dir)
            obstacles = newPos:remove  (obstacle env1) coord
            emptys = coord:remove (empty env1) newPos 
            env2 = env { obstacle = obstacles, empty = emptys}
            
        in  (env2,True )
    | otherwise = (env,False )
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
                 if freeCellsCount <= 3 then
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
                

                    
              
        (dirtyCells,gen) =  assertProb dirtProb dirtTargets (randomGen env)
        dirtyCells2 = dirtyCells++(dirty env)
        emptyOld = empty env
        emptyNew = filter (`notElem` dirtyCells2) emptyOld
    in env{dirty = dirtyCells2,empty = emptyNew }
      
-------------------------------------------------------------


----Robot Actions-----

moveRobot ::Environment -> Coord ->Coord -> Environment
moveRobot env src dest = 
    let 
        emptyOld = empty env
        emptyNew = src:(remove emptyOld dest)
        newRobot = robot env
    in env{robot = newRobot{pos = dest} ,empty = emptyNew}

moveAndTake ::Environment -> Coord ->Coord -> Environment
moveAndTake env src dest = 
    let 
        emptyOld = empty env
        emptyNew = src:(remove emptyOld dest)
        kidsOld = kids env
        kidsNew = remove kidsOld (initKid dest)
        newRobot = robot env
    in env {robot = newRobot{pos = dest , isHoldingKid =True},kids = kidsNew , empty = emptyNew}

drop::Environment -> Coord  -> Environment
drop env coord = 
    let 
        newKids =initKid coord:(kids env)
        newRobot = robot env
        newCrib = remove (crib env) coord
        newFullCrib = coord:(fullCrib env)
    in if isCrib env coord then env{robot = newRobot{isHoldingKid = False},crib = newCrib ,fullCrib = newFullCrib} else env {robot = newRobot{isHoldingKid = False}, kids = newKids}

clean::Environment -> Coord  -> Environment
clean env coord = 
    let
        newDirt =remove (dirty env) coord
    in env {dirty = newDirt}
----------------------

-----Checkers----------------------

isRobot :: Environment -> Coord  -> Bool
isRobot env coord = pos (robot env) ==coord && not (isHoldingKid (robot env))

isRobotWithKid :: Environment -> Coord  -> Bool
isRobotWithKid  env coord = pos (robot env) ==coord &&  (isHoldingKid (robot env))

isEmpty :: Environment -> Coord  -> Bool
isEmpty env coord = coord `elem` empty env

isKid :: Environment -> Coord  -> Bool
isKid  env coord= coord `elem` (map position (kids env)) 

isCrib :: Environment -> Coord  -> Bool
isCrib  env coord= coord `elem` crib  env 

isFullCrib :: Environment -> Coord  -> Bool
isFullCrib  env coord= coord `elem` fullCrib  env 

isDirty ::Environment -> Coord  -> Bool
isDirty  env coord= coord `elem` dirty  env 

isObstacle :: Environment -> Coord  -> Bool
isObstacle  env coord= coord `elem` obstacle  env 

----------------------------------


-----IO-------------

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

---------------------------------
