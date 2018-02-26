module PolyhedraIntersection
  where
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (pokeArray)
import           Foreign.Storable      (peek, sizeOf)
import           Helpers               (unmakeMesh)
import           Mesh
import           Types

polyhedraIntersection :: Mesh -> Mesh -> IO Mesh
polyhedraIntersection mesh1 mesh2 = do
  let mesh1' = unmakeMesh mesh1
      mesh2' = unmakeMesh mesh2
  polyhedraIntersection_ mesh1' mesh2'

polyhedraIntersection_ :: ([[Double]],[[Int]]) -> ([[Double]],[[Int]]) -> IO Mesh
polyhedraIntersection_ (vertices1, faces1) (vertices2, faces2) = do
  let nvertices1 = length vertices1
      nfaces1 = length faces1
      facesizes1 = map length faces1
      l1 = sum facesizes1
      nvertices2 = length vertices2
      nfaces2 = length faces2
      facesizes2 = map length faces2
      l2 = sum facesizes2
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
  putStrLn "run C++"
  meshPtr <- c_polyhedraIntersection vertices1Ptr (fromIntegral nvertices1) faces1Ptr
                                     facesizes1Ptr (fromIntegral nfaces1)
                                     vertices2Ptr (fromIntegral nvertices2) faces2Ptr
                                     facesizes2Ptr (fromIntegral nfaces2)
  cmesh <- peek meshPtr
  free vertices1Ptr
  free faces1Ptr
  free facesizes1Ptr
  free vertices2Ptr
  free faces2Ptr
  free facesizes2Ptr
  cMeshToMesh cmesh
