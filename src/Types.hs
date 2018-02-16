module Types
  where
import Data.IntMap.Strict (IntMap)

data Vertex3 = Vertex3 Double Double Double
     deriving (Show, Eq)

data Face = Face {
    _verticesIds :: [Int]
} deriving (Show, Eq)

data Mesh = Mesh {
    _vertices :: IntMap Vertex3
  , _faces :: [Face]
} deriving Show
