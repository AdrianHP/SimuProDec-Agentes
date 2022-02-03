module Environment where
import Data

import Control.Monad (guard)
import Debug.Trace (trace)
import Utils
import System.Random
import Kid
import Robot
import GHC.Base (compareWord)


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
    , robot = Robot{pos = (0,0), isHoldingKid = False ,prevPos= (0,0) }
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
    , robot = Robot{pos = (0,0), isHoldingKid = False,prevPos= (0,0)}
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
        elem = emptyOld!!rnd
        emptyNew = remove emptyOld elem
        itemNew = Robot elem False elem
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
        elem = emptyOld!!rnd
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
        elem = emptyOld!!rnd
        emptyNew = remove emptyOld elem
        itemNew = initKid elem: kids env
        env2 = env{empty =emptyNew , kids = itemNew , randomGen = gen}

    in generateKids env2  (items-1)

generateDirt:: Environment -> Int -> Environment
generateDirt env 0  = env
generateDirt env items =
    let
        emptyOld = empty env
        (rnd,gen) = myRandom 0 (length' emptyOld) (randomGen env)
        elem = emptyOld!!rnd
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
        envAfterMove = foldl (\ env k -> moveKid env k (dirtProb env)) env kidsToMove
        i =  trace("kids to move" ++show kidsToMove ++ show envAfterMove)
    in envAfterMove



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
    in env{robot = newRobot{pos = dest,prevPos = src} ,empty = emptyNew}

moveAndTake ::Environment -> Coord ->Coord -> Environment
moveAndTake env src dest =
    let
        emptyOld = empty env
        emptyNew = src:(remove emptyOld dest)
        kidsOld = kids env
        kidsNew = remove kidsOld (initKid dest)
        newRobot = robot env
    in env {robot = newRobot{pos = dest , isHoldingKid =True ,prevPos = src},kids = kidsNew , empty = emptyNew}

dropKid::Environment -> Coord  -> Environment
dropKid env coord =
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

----ReflexRobot--------------

chooseAction::Environment ->Bool  -> Environment
chooseAction env kidPriority 
    | clean && not loaded =trace "Ambiente sin suciedad y bebes en el corral,nada que hacer" env
    | otherwise =
                let
                    

                in if loaded then loadedAction env kidPriority False  else  unloadedAction  env kidPriority
    where 
    loaded = isHoldingKid (robot env)
    clean = length' (kids env) == 0 && length' (dirty env) ==0





unloadedAction:: Environment -> Bool -> Environment
unloadedAction env kidPriorityIn =
    let
        kidPriority = if kidPriorityIn &&length' (kids env) == 0 || not kidPriorityIn && length' (dirty env) ==0 
                            then not kidPriorityIn 
                      else kidPriorityIn


        actualPos =pos (robot env)
        neighbors = findNeighbors actualPos ([size env])
        anyKid =  filter (isKid env) neighbors
        anyDirt = filter (isDirty env) neighbors
        
        bestPosibleMoves  
                | (length' anyKid > 0 && kidPriority) || (length' anyDirt > 0 && not kidPriority) =(0,0)
                | anyEnvTarget = (makePath [] closeTargetParents  closeTargetCoord (fst (size env) ))!!1
                | length' temp >0 =  temp!!0
                | otherwise  = (-1,-1)
                where
                    (closeItems,parents,parentsKid) =   closestObjects env actualPos
                    closeTargetCoord =if kidPriority  then closeItems!!0 else closeItems!!1
                    closeTargetParents = if kidPriority then parentsKid else parents
                    anyEnvTarget = closeTargetCoord /= (-1,-1)
                    temp = filter (canMove env ) neighbors
        fullCribs = filter (isFullCrib env) neighbors

        (str,resultEnv )
                 | isDirty env actualPos = ( "Limpia", clean env actualPos)
                 | length' anyKid > 0 && kidPriority  = ( "Carga bebe" , moveAndTake env actualPos (anyKid!!0) )
                 | length' anyDirt > 0 && not kidPriority  = ( "Se mueve para lugar con suciedad" , moveRobot env actualPos (anyDirt!!0) )
                 | bestPosibleMoves /=(-1,-1)  = ( "Busca "++if kidPriority then "el bebe" else "la suciedad" ++ " mas cercan@"  , moveRobot env actualPos bestPosibleMoves)
                 | length' fullCribs > 0 =( "No puede moverse,la unica opcion es meterse en un carral y cargar bebe" , moveAndTake env actualPos (fullCribs!!0) )
                 | otherwise =  ( "No se puede mover" ,env)



        in trace (str++". Se mueve de "++ show actualPos++" ->"++show (pos (robot resultEnv))   ) resultEnv


loadedAction:: Environment -> Bool -> Bool  -> Environment
loadedAction env kidPriority stop =
    let
        actualPos =pos (robot env)
        neighbors = findNeighbors actualPos ([size env])
        anyCrib = filter (isCrib env) neighbors
        
        bestPosibleMoves 
                |length' anyCrib>0 = (0,0)
                | anyEnvTarget = (makePath [] closeTargetParents  closeTargetCoord (fst (size env) ))!!1
                | length' temp >0 =  temp!!0
                | otherwise  = (-1,-1)

                where
                    (closeItems,parents,parentsKid) =  closestObjects env actualPos
                    closeTargetCoord = closeItems!!2
                    closeTargetParents = parents
                    anyEnvTarget = closeTargetCoord /= (-1,-1)
                    temp = filter (canMove env ) neighbors
              

        (str,resultEnv )
                 | isDirty env actualPos && not stop  = ( "Limpia", clean env actualPos)
                 | isCrib env actualPos && not stop  = ( "Deja al bebe en el corral", dropKid env actualPos)
                 | length' anyCrib > 0   =  ( "Se mueve para un corral" , moveAndTake env actualPos (anyCrib!!0) )
                 | bestPosibleMoves /=(-1,-1)  = if not stop  then 
                                                    let
                                                        env2 = moveRobot env actualPos bestPosibleMoves
                                                     in ("Busca el corrarl mas cecano y se puede seguir moviendo", loadedAction env2 kidPriority True) 
                                                 else ( "Busca el corral mas cercano ya no se puede mover mas"  , moveRobot env actualPos bestPosibleMoves)
                 | otherwise =  ( "Deja al bebe" ,env)

        in trace (str++ ". Se mueve de "++ show actualPos++" ->"++ show (pos (robot resultEnv))  ) resultEnv
-----------------------------




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

canMove :: Environment -> Coord  -> Bool
canMove env coord = not (isObstacle env coord || isFullCrib env coord || isKid env coord)
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


---Utils-----------------

closestObjects::Environment -> Coord -> ( [Coord],[Coord],[Coord])
closestObjects env coord =
    let
        (m,n) = size env
        (p,parents) = bfsPath env [coord] [] [(m,n)] (emptyList (m*n)) (m*n)  --bfsKid env [(0,0)] [] [(5,5)] (emptyList 25) 25
        path = remove p coord
        (pKid,parentsKid) = bfsKid env [coord] [] [(m,n)] (emptyList (m*n)) (m*n)
        pathKid = remove pKid coord

        closestDirt = fstMatch env path (length' path -1) isDirty
        closestCrib = fstMatch env path (length' path -1) isCrib
        closestKid = fstMatch env pathKid ( length' pathKid -1) isKid

     in ([closestKid,closestDirt,closestCrib],parents,parentsKid)

fstMatch::Environment -> [Coord] -> Int -> (Environment -> Coord  -> Bool) -> Coord
fstMatch _ _ (-1) _ = (-1,-1)
fstMatch env list index predicate =
    let
        coord = list!!index
    in if predicate env coord then coord  else fstMatch env list (index-1) predicate

absClose :: Coord -> Coord -> Coord  -> Coord
absClose (i,j) (k,l) (x,y) | absCoord1X > absCoord2X =(k,l)
                           | absCoord1X < absCoord2X = (i,j)
                           | absCoord1Y > absCoord2Y = (k,l)
                           | otherwise = (i,j)

    where
        absCoord1X= abs (i-x)
        absCoord2X= abs (k-x)
        absCoord1Y= abs (j-y)
        absCoord2Y = abs (l-y)



bfsPath::Environment -> [Coord ] -> [Coord ] -> [Coord ] ->[Coord] ->Int ->([Coord],[Coord])
bfsPath env [] visit limits path count = (visit,path)
bfsPath env q visit _ path 0 = (visit,path)
bfsPath env  (q:rest) visit limits path count
                            | q `elem` visit = bfsPath env rest visit limits path count
                            | otherwise =
                                let
                                    v = q:visit
                                    (m,_) = size env
                                    n= filter  ( \temp->temp `notElem` v && canMove env temp) (findNeighbors  q limits)
                                    neiToInt = map  (\(x,y)-> x*m+y)  n
                                    updateP = updatePath [] path neiToInt q ((length' path)-1 )
                                    neighbors = rest ++ n --trace("index"++show neiToInt++" " ++show q ++show updateP )
                                in  bfsPath env neighbors v limits updateP (count-1)

bfsKid::Environment -> [Coord ] -> [Coord ] -> [Coord ] ->[Coord] ->Int ->([Coord],[Coord])
bfsKid env [] visit limits path count = (visit,path)
bfsKid env q visit _ path 0 = (visit,path)
bfsKid env  (q:rest) visit limits path count
                            | q `elem` visit = bfsKid env rest visit limits path count
                            | otherwise =
                                let
                                    v = q:visit
                                    (m,_) = size env
                                    t = filter  ( isKid env) (findNeighbors  q limits)
                                    anyKid = length' t>0
                                    n= if anyKid then [t!!0] else  filter  ( \temp->temp `notElem` v && canMove env temp) (findNeighbors  q limits)
                                    neiToInt = map  (\(x,y)-> x*m+y)  n
                                    updateP = updatePath [] path neiToInt q ((length' path)-1 )
                                    neighbors = rest ++ n --trace("index"++show neiToInt++" " ++show q ++show updateP )
                                in if anyKid then bfsKid env neighbors (t!!0:v) limits updateP 0 else bfsKid env neighbors v limits updateP (count-1)







updatePath::[Coord] ->[Coord]->[Int]->Coord -> Int->[Coord]
updatePath result _ _ _ (-1) = result
updatePath result (p:path) nIndex c len =
    if len `elem` nIndex && ((p:path)!!len) ==(-1,-1) then
        updatePath (c:result) (p:path) nIndex c (len-1)
    else
        updatePath (((p:path))!!len:result) (p:path) nIndex c (len-1)


makePath:: [Coord] -> [Coord] -> Coord ->Int -> [Coord]
makePath  result path (x,y) m = if path!!index==(-1,-1) then result else makePath (newCoord:result) path newCoord m
                         where
                             index = x*m+y
                             newCoord = path!!index


-------------------------