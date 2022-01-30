module Environment where
import Data 

import Control.Monad (guard)
import Debug.Trace (trace)
import Utils
import Text.ParserCombinators.ReadP (count)

-- import Utils



emptyEnvironment :: Environment
emptyEnvironment = Environment { 
    size = (0,0)
    , robot = (0,0)
    , robotWithKid = (0,0)
    , kids = ([])
    , crib = ([])
    , fullCrib = ([])
    , dirty = ([])
    , obstacle = ([])
    , empty = ([])
    }

-- generateEnvironment :: m -> n -> kids -> obstaclesPercentage -> dirtPercentage -> dirtProb -> moveKidProb -> Environment





generateCrib :: Environment -> Int -> Int ->Int  -> Environment
generateCrib env  m n kids  = 
    let count = kids
        srcX = myRandom 0 (n-1)
        srcY = myRandom 0 (m-1)
    in env{crib  = bfs [(srcX,srcY)] [(m,n)] kids  } 

generateObstacles :: Environment ->Int -> Int -> Int -> Environment 
generateObstacles =generateObstacles2 []

generateObstacles2::[Coord] -> Environment ->Int -> Int -> Int -> Environment 
generateObstacles2 obsL env m n 0  = env{obstacle=obsL}
generateObstacles2 obsL env m n obs = 
    let count = obs   
        x = myRandom 0 (n-1)
        y = myRandom 0 (m-1)
        sy = trace (show x ++" " ++show y)
    in if (x,y) `notElem` crib  env then generateObstacles2 ((x,y):obsL) env m n (obs-1) else  generateObstacles2 obsL env m n obs






