module Data where
import System.Random

type Coord = (Int, Int)

data Environment = Environment {
                    size    :: Coord
                ,   robot  :: Coord
                ,   robotWithKid  :: Coord
                ,   isHoldingKid ::Bool
                ,   kids   :: [Coord]
                ,   crib  :: [Coord]
                ,   fullCrib :: [Coord]
                ,   dirty :: [Coord]
                ,   obstacle :: [Coord]
                ,   empty ::[Coord]
                ,   remaininKids :: Int 
                ,   moveKidProb :: Int
                ,   dirtProb :: Int
                ,  randomGen :: StdGen
                } deriving (Eq ,Show)




data Cell = Kid
          | Robot
          | RobotWithKid
          | Empty
          | Crib 
          | FullCrib
          | Dirty 
          | Obstacle  deriving (Show)


data Moves = Up 
             | Down 
             | L 
             | R 
             | UpLeft 
             | UpRight
             | DownLeft
             | DownRight
             | UpUp 
             | DownDown
             | LeftLeft 
             | RightRight deriving (Show)