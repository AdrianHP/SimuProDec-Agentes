
module Kid where

import Data

data Kid = Kid{
            position :: Coord
        ,   isLoaded :: Bool
        ,   inCrib :: Bool

}deriving (Eq ,Show)

initKid ::Coord->Kid
initKid coord = Kid coord False False 