module Helpers
  where
import qualified Data.IntMap.Strict as IM
import           Data.List             (union)
import           Data.Permute          (elems, rank)
import           Types

makeMesh :: ([[Double]], [[Int]]) -> Mesh
makeMesh (vertices, faces) =
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

unmakeMesh :: Mesh -> ([[Double]], [[Int]])
unmakeMesh mesh = (vertices, faces)
  where
  vertices' = IM.elems (_vertices mesh)
  vertices = map fromVertex3 vertices'
    where
    fromVertex3 (Vertex3 x y z) = [x,y,z]
  faces = map _verticesIds (_faces mesh)

-- | given a list of vertices and a list of faces, this returns the list of
-- vertices involved in the faces and the new indices of the faces, i.e. the
-- indices with respect to the new list of vertices
fixIndices :: [[Double]] -> [[Int]] -> ([[Double]], [[Int]])
fixIndices allVertices faces = (newvertices, newfaces)
  where
  faceselems = foldr union [] faces
  l = length faceselems
  permute = elems $ rank l faceselems
  mapper = IM.fromList $ zip permute faceselems
  mapper' = IM.fromList $ zip faceselems permute
  newfaces = map (map ((IM.!) mapper')) faces
  newvertices = [allVertices !! (mapper IM.! i) | i <- [0 .. l-1]]

-- | round the vertices of a mesh
roundVertices :: Int -> Mesh -> Mesh
roundVertices n mesh = Mesh {
                              _vertices = vertices'
                            , _faces = _faces mesh
                            , _edges = _edges mesh
                            }
  where
    vertices' = IM.map approxVx3 (_vertices mesh)
      where
        approxVx3 (Vertex3 u v w) = Vertex3 (approx u) (approx v) (approx w)
        approx x = fromInteger (round $ x * (10^n)) / (10.0^^n)
