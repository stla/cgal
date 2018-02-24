module PolyhedraUnion
  where
import           Control.Monad         (forM_)
import           Foreign               (Ptr, alloca)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (peekArray, pokeArray)
import           Foreign.Storable      (peek, poke, sizeOf)
import           Helpers               (unmakeMesh)
import           Mesh
import           Types
import Examples

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

polyhedraUnions :: [([[Double]], [[Int]])] -> IO Mesh
polyhedraUnions polyhedras = do
  cpolyhedras <- makeCPolyhedra polyhedras
  let npolyhedras = length polyhedras
  cpolyhedrasPtr <- mallocBytes (npolyhedras * sizeOf (undefined :: CPolyhedron))
  pokeArray cpolyhedrasPtr cpolyhedras
  cmeshPtr <- c_unionNpolyhedra cpolyhedrasPtr (fromIntegral npolyhedras)
  cmesh <- peek cmeshPtr
  cMeshToMesh cmesh

polyhedraUnion_ :: ([[Double]],[[Int]]) -> ([[Double]],[[Int]]) -> ([[Double]],[[Int]]) -> IO Mesh
polyhedraUnion_ (vertices1, faces1) (vertices2, faces2) (vertices3, faces3) = do
  let nvertices1 = length vertices1
      nfaces1 = length faces1
      facesizes1 = map length faces1
      l1 = sum facesizes1
      nvertices2 = length vertices2
      nfaces2 = length faces2
      facesizes2 = map length faces2
      l2 = sum facesizes2
      nvertices3 = length vertices3
      nfaces3 = length faces3
      facesizes3 = map length faces3
      l3 = sum facesizes3
  vertices1Ptr <- mallocBytes (nvertices1 * 3 * sizeOf (undefined :: CDouble))
  pokeArray vertices1Ptr (concatMap (map realToFrac) vertices1)
  faces1Ptr <- mallocBytes (l1 * sizeOf (undefined :: CInt))
  pokeArray faces1Ptr (concatMap (map fromIntegral) faces1)
  facesizes1Ptr <- mallocBytes (nfaces1 * sizeOf (undefined :: CInt))
  pokeArray facesizes1Ptr (map fromIntegral facesizes1)
  vertices2Ptr <- mallocBytes (nvertices2 * 3 * sizeOf (undefined :: CDouble))
  pokeArray vertices2Ptr (concatMap (map realToFrac) vertices2)
  faces2Ptr <- mallocBytes (l2 * sizeOf (undefined :: CInt))
  pokeArray faces2Ptr (concatMap (map fromIntegral) faces2)
  facesizes2Ptr <- mallocBytes (nfaces2 * sizeOf (undefined :: CInt))
  pokeArray facesizes2Ptr (map fromIntegral facesizes2)
  vertices3Ptr <- mallocBytes (nvertices3 * 3 * sizeOf (undefined :: CDouble))
  pokeArray vertices3Ptr (concatMap (map realToFrac) vertices3)
  faces3Ptr <- mallocBytes (l3 * sizeOf (undefined :: CInt))
  pokeArray faces3Ptr (concatMap (map fromIntegral) faces3)
  facesizes3Ptr <- mallocBytes (nfaces3 * sizeOf (undefined :: CInt))
  pokeArray facesizes3Ptr (map fromIntegral facesizes1)
  meshPtr <- c_polyhedraUnion vertices1Ptr (fromIntegral nvertices1) faces1Ptr
                              facesizes1Ptr (fromIntegral nfaces1)
                              vertices2Ptr (fromIntegral nvertices2) faces2Ptr
                              facesizes2Ptr (fromIntegral nfaces2)
                              vertices3Ptr (fromIntegral nvertices3) faces3Ptr
                              facesizes3Ptr (fromIntegral nfaces3)
  cmesh <- peek meshPtr
  free vertices1Ptr
  free faces1Ptr
  free facesizes1Ptr
  free vertices2Ptr
  free faces2Ptr
  free facesizes2Ptr
  free vertices3Ptr
  free faces3Ptr
  free facesizes3Ptr
  cMeshToMesh cmesh

test3cubes :: IO Mesh
test3cubes =
  polyhedraUnion_ (cubeVertices1, cubeTriFaces1) (cubeVertices2, cubeTriFaces2)
                  (cubeVertices3, cubeTriFaces3)
