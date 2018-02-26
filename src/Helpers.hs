module Helpers
  where
import qualified Data.IntMap.Strict as IM
import           Data.List             (union)
import           Data.Permute          (elems, rank)
import           Types
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (pokeArray)
import           Foreign.Storable      (peek, sizeOf)
import Mesh

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


-- polyhedraUnion :: [([[Double]],[[Int]])] -> IO Mesh
-- polyhedraUnion verticesFaces = do
--   let verticesList = map fst verticesFaces
--       facesList = map snd verticesFaces
--       nverticesList = map length verticesList
--       facesizesList = map (map length) facesList
--       totalFacesizesList = map sum facesizesList
--   return zoubi

dbl2Cdbl :: [[Double]] -> [CDouble]
dbl2Cdbl = concatMap (map realToFrac)
int2Cint :: [[Int]] -> [CInt]
int2Cint = concatMap (map fromIntegral)

-- polyhedraUnion :: [([[Double]],[[Int]])] -> IO Mesh
-- polyhedraUnion verticesFaces = do
--   let verticesList = map fst verticesFaces
--       facesList = map snd verticesFaces
--       nverticesList = map length verticesList
--       facesizesList = map (map length) facesList
--       totalFacesizesList = map sum facesizesList
--   return zoubi

makeCPolyhedron :: ([[Double]], [[Int]]) -> IO CPolyhedron
makeCPolyhedron (vertices, faces) = do
  let nvertices = length vertices
      facesizes = map length faces
      totalFacesizes = sum facesizes
      nfaces = length faces
  vsPtr <- mallocBytes (nvertices * 3 * sizeOf (undefined :: CDouble))
  pokeArray vsPtr (dbl2Cdbl vertices)
  fsPtr <- mallocBytes (totalFacesizes * sizeOf (undefined :: CInt))
  pokeArray fsPtr (int2Cint faces)
  fssPtr <- mallocBytes (nfaces * sizeOf (undefined :: CInt))
  pokeArray fssPtr (map fromIntegral facesizes)
  let cp = CPolyhedron
            { __vertices' = vsPtr
            , __nvertices'' = fromIntegral nvertices
            , __faces' = fsPtr
            , __facesizes' = fssPtr
            , __nfaces' = fromIntegral nfaces
            }
  return cp

makeCPolyhedra :: [([[Double]], [[Int]])] -> IO [CPolyhedron]
makeCPolyhedra = mapM makeCPolyhedron
