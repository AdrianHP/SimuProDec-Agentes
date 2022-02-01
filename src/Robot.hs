module Robot where
import Control.Lens.At
import Data

data Robot = Robot{
            pos :: Coord
        ,   isHoldingKid :: Bool

}deriving (Eq ,Show)
