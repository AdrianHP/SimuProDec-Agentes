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
                ,  kidPriorityRobot::Bool
                ,  modelRobot::Int
                , testMode :: Bool 
                } deriving (Eq ,Show)

-------------------------------------------


-- Generator-------------------------------

-- setEnvironment ::  Environment
-- setEnvironment  = Environment {
--     size = (4,5)
--     , robot = Robot{pos = (0,0), isHoldingKid = False ,prevPos= (0,0) }
--     , kids = [initKid (2,1),initKid (2,3)]
--     , crib = []
--     , fullCrib = []
--     , dirty = []
--     , obstacle = [(1,2)]
--     , empty =  [(0,1),(1,1),(0,2),(0,3),(0,4),(1,0),(1,4),(2,0),(1,3),(2,2),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4)]
--     , dirtProb = 100
--     , moveKidProb = 10
--     , remaininKids = 1
--     , randomGen =mkStdGen 10
--     , time = 0
--     , turn = 1
--     , testMode = False 

--     }

generateEnvironment :: Int -> Int -> Int -> Int -> Int -> Int -> Int ->Int -> Int -> Bool -> Int-> Bool -> Environment 
generateEnvironment m n kids obstaclesPercentage dirtPercentage dirtProb moveKidProb t rndSeed kidPriority model test =
    let
        totalObst = div (obstaclesPercentage * m * n)  100
        totalDirt = div  ( dirtPercentage * m * n )  100
        empty = emptyEnvironment m n dirtProb moveKidProb t rndSeed  kidPriority model test
        crib = generateCrib empty m n kids
        obs =   generateObstacles crib totalObst
        dirt = generateDirt obs  totalDirt
        kid = generateKids dirt  kids
        env = putRobot kid
    in  env

emptyEnvironment ::Int -> Int -> Int -> Int -> Int -> Int ->Bool ->Int -> Bool -> Environment
emptyEnvironment m n dirtProb moveKidProb t rndSeed  kidPriority model test = Environment {
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
    , turn = 1
    , kidPriorityRobot = kidPriority
    , modelRobot = model
    , testMode =test
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

passTime::Environment -> Environment
passTime env = if testMode env then trace("turno " ++show (turn env) ) env3{ turn = (turn env)+1} else env3{ turn = (turn env)+1}
    where
        (_,var) = divMod (turn env) (time env)
        variate = var==0 && (turn env) >0
        env1 = robotAction env (kidPriorityRobot env) (modelRobot env)
        env2 = kidsActions env1
        

        env3 =if  variate then  variateEnvironment env2 else env2


variateEnvironment::Environment -> Environment
variateEnvironment env =if testMode env then  trace"El ambiente ha variado aleatoriamente"  env else env

success::Environment -> (Bool,Float)
success env = (dirtPercent<40,dirtPercent)
    where
        dirt = length' (dirty env)
        empt = length' (empty env)
        dirtPercent = dirt *100/(dirt+empt)
--------------------------------



-----Kids Actions----------------

kidsActions::Environment -> Environment
kidsActions env =
    let
        moveProb = moveKidProb env
        (temp,gen) = assertProb moveProb (kids env) (randomGen env)
        kidsToMove = map position temp
        envAfterMove = foldl (\ env k -> moveKid env k (dirtProb env)) env kidsToMove
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
            in if canMove then  envIfCanMovPutDirt{randomGen = gen} else  env{randomGen = gen}
        else if isEmpty env newPos then
            let
              emptyOld = empty env
              emptyNew = (x,y):(remove emptyOld newPos)
              kidsOld = kids env
              kidsNew = (initKid newPos):(remove kidsOld (initKid (x,y)))
              env2 = env{kids =kidsNew , empty = emptyNew}
              envPutDirt = putDirt env2 (x,y) newPos dirtProb
            in   envPutDirt{randomGen = gen}
            else env{randomGen = gen}
    else  env{randomGen = gen}


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

robotAction::Environment ->Bool ->Int  -> Environment
robotAction env kidPriority model
    | clean && not loaded =if (testMode env) then trace "Ambiente sin suciedad y bebes en el corral,nada que hacer" env else env
    | otherwise = if loaded then loadedAction env kidPriority False model  else  unloadedAction  env kidPriority model
    where 
    loaded = isHoldingKid (robot env)
    clean = length' (kids env) == 0 && length' (dirty env) ==0





unloadedAction:: Environment -> Bool ->Int-> Environment
unloadedAction env kidPriorityIn model =
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
                | anyEnvTarget = (makePath [] closeTargetParents  closeTargetCoord (snd (size env) ))!!1
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
                 | isDirty env actualPos  && ( model ==0 ||not kidPriority) = ( "Limpia", clean env actualPos)
                 | length' anyKid > 0  = ( "Se mueve hacia el bebe adyacente y lo carga" , moveAndTake env actualPos (anyKid!!0) )
                 | length' anyDirt > 0 && not kidPriority  = ( "Se mueve para lugar con suciedad" , moveRobot env actualPos (anyDirt!!0) )
                 | bestPosibleMoves /=(-1,-1)  = ( "Busca "++if kidPriority then "el bebe mas cercano" else "la suciedad mas cercana"  , moveRobot env actualPos bestPosibleMoves)
                 | isDirty env actualPos   = ( "Limpia", clean env actualPos)
                 | length' fullCribs > 0 =( "No puede moverse,la unica opcion es meterse en un corral y cargar bebe" , moveAndTake env actualPos (fullCribs!!0) )
                 | otherwise =  ( "No se puede mover" ,env)



        in if (testMode env) then trace (str++"......... src -> dest "++ show actualPos++" ->"++show (pos (robot resultEnv))   ) resultEnv else resultEnv


loadedAction:: Environment -> Bool -> Bool ->Int  -> Environment
loadedAction env kidPriority stop model =
    let
        actualPos =pos (robot env)
        neighbors = findNeighbors actualPos ([size env])
        anyCrib = filter (isCrib env) neighbors
        
        bestPosibleMoves 
                |length' anyCrib>0 = (0,0)
                | anyEnvTarget = (makePath [] closeTargetParents  closeTargetCoord (snd (size env) ))!!1
                | length' temp >0 =  temp!!0
                | otherwise  = (-1,-1)

                where
                    (closeItems,parents,parentsKid) =  closestObjects env actualPos
                    closeTargetCoord = closeItems!!2
                    closeTargetParents = parents
                    anyEnvTarget = closeTargetCoord /= (-1,-1)
                    temp = filter (canMove env ) neighbors
              

        (str,resultEnv )
                 | isDirty env actualPos && not stop &&model==0  = ( "Limpia", clean env actualPos)
                 | isCrib env actualPos && not stop  = ( "Deja al bebe en el corral", dropKid env actualPos)
                 | length' anyCrib > 0   =  ( "Se mueve para un corral" , moveAndTake env actualPos (anyCrib!!0) )
                 | bestPosibleMoves /=(-1,-1)  = if not stop  then 
                                                    let
                                                        env2 = moveRobot env actualPos bestPosibleMoves
                                                     in ("Busca el corrarl mas cecano y se puede seguir moviendo", loadedAction env2 kidPriority True model) 
                                                 else ( "Busca el corral mas cercano ya no se puede mover mas"  , moveRobot env actualPos bestPosibleMoves)
                 | isDirty env actualPos && not stop  = ( "Limpia", clean env actualPos)
                 | otherwise =  ( "Deja al bebe" ,env)

        in if (testMode env) then trace (str++"......... src -> dest "++ show actualPos++" ->"++show (pos (robot resultEnv))   ) resultEnv else resultEnv

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


  



bfsPath::Environment -> [Coord ] -> [Coord ] -> [Coord ] ->[Coord] ->Int ->([Coord],[Coord])
bfsPath env [] visit limits path count = (visit,path)
bfsPath env q visit _ path 0 = (visit,path)
bfsPath env  (q:rest) visit limits path count
                            | q `elem` visit = bfsPath env rest visit limits path count
                            | otherwise =
                                let
                                    v = q:visit
                                    (_,columns) = size env
                                    n= filter  ( \temp->temp `notElem` v && canMove env temp) (findNeighbors  q limits)
                                    neiToInt = map  (\(x,y)-> x*columns+y)  n
                                    updateP = updatePath [] path neiToInt q ((length' path)-1 )
                                    neighbors = rest ++ n 
                                in  bfsPath env neighbors v limits updateP (count-1)

bfsKid::Environment -> [Coord ] -> [Coord ] -> [Coord ] ->[Coord] ->Int ->([Coord],[Coord])
bfsKid env [] visit limits path count = (visit,path)
bfsKid env q visit _ path 0 = (visit,path)
bfsKid env  (q:rest) visit limits path count
                            | q `elem` visit = bfsKid env rest visit limits path count
                            | otherwise =
                                let
                                    v = q:visit
                                    (_,columns) = size env
                                    t = filter  ( isKid env) (findNeighbors  q limits)
                                    anyKid = length' t>0
                                    n= if anyKid then [t!!0] else  filter  ( \temp->temp `notElem` v && canMove env temp) (findNeighbors  q limits)
                                    neiToInt = map  (\(x,y)-> x*columns+y)  n
                                    updateP = updatePath [] path neiToInt q ((length' path)-1 )
                                    neighbors = rest ++ n 
                                in if anyKid then bfsKid env neighbors (t!!0:v) limits updateP 0 else bfsKid env neighbors v limits updateP (count-1)








-------------------------