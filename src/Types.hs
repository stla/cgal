module Types
  where
import           Data.IntMap.Strict (IntMap)

data Vertex3 = Vertex3 Double Double Double
     deriving (Show, Eq)

data Face = Face {
    _verticesIds :: [Int]
} deriving (Show, Eq)

data IndexPair = Pair Int Int
  deriving (Show, Read)
instance Eq IndexPair where
    Pair i j == Pair i' j' = (i == i' && j == j') || (i == j' && j == i')

data Mesh = Mesh {
    _vertices :: IntMap Vertex3
  , _faces    :: [Face]
  , _edges    :: Maybe [IndexPair]
} deriving Show
