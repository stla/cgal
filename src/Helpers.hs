module Helpers
  where
import qualified Data.IntMap.Strict as IM
import           Types

makeMesh :: [[Double]] -> [[Int]] -> Mesh
makeMesh vertices faces =
  Mesh {  _vertices = vertex3map
        , _faces = faces'
        , _edges = Nothing }
  where
  vertices' = map toVertex3 vertices
    where
    toVertex3 v = Vertex3 (v!!0) (v!!1) (v!!2)
  vertex3map = IM.fromAscList (zip [0 .. length vertices - 1] vertices')
  faces' = map toFace faces
    where
    toFace idxs = Face { _verticesIds = idxs }
