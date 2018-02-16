module Types
  where

data Vertex3 = Vertex3 Double Double Double
     deriving (Show, Eq)

data Face = Face {
    _verticesIds :: [Int]
} deriving (Show, Eq)

data Mesh = Mesh {
    _vertices :: [Vertex3]
  , _faces :: [Face]
} deriving Show
