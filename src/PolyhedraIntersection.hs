module PolyhedraIntersection
  where
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (pokeArray)
import           Foreign.Storable      (peek, sizeOf)
import           Mesh
import           Types

polyhedraIntersection :: ([[Double]],[[Int]]) -> ([[Double]],[[Int]]) -> IO Mesh
polyhedraIntersection (vertices1, faces1) (vertices2, faces2) = do
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
  meshPtr <- c_polyhedraIntersection vertices1Ptr (fromIntegral nvertices1) faces1Ptr
                                     facesizes1Ptr (fromIntegral nfaces1)
                                     vertices2Ptr (fromIntegral nvertices2) faces2Ptr
                                     facesizes2Ptr (fromIntegral nfaces2)
  free vertices1Ptr
  free faces1Ptr
  free facesizes1Ptr
  free vertices2Ptr
  free faces2Ptr
  free facesizes2Ptr
  cmesh <- peek meshPtr
  cMeshToMesh cmesh

test :: IO Mesh
test = do
  let vs1 = [ [-1,-1,-1],
              [-1,-1, 1],
              [-1, 1,-1],
              [-1, 1, 1],
              [ 1,-1,-1],
              [ 1,-1, 1],
              [ 1, 1,-1],
              [ 1, 1, 1] ]
      faces1 =  [ [ 0 , 2 , 6 , 4 ]
                , [ 4 , 5 , 1 , 0 ]
                , [ 6 , 7 , 5 , 4 ]
                , [ 0 , 1 , 3 , 2 ]
                , [ 2 , 3 , 7 , 6 ]
                , [ 5 , 7 , 3 , 1 ]
                ]
      vs2 = [ [ 0.0 , -1.0 , -1.618033988749895 ]
            , [ 0.0 , -1.0 , 1.618033988749895 ]
            , [ 0.0 , 1.0 , -1.618033988749895 ]
            , [ 0.0 , 1.0 , 1.618033988749895 ]
            , [ -1.618033988749895 , 0.0 , -1.0 ]
            , [ 1.618033988749895 , 0.0 , -1.0 ]
            , [ -1.618033988749895 , 0.0 , 1.0 ]
            , [ 1.618033988749895 , 0.0 , 1.0 ]
            , [ -1.0 , -1.618033988749895 , 0.0 ]
            , [ -1.0 , 1.618033988749895 , 0.0 ]
            , [ 1.0 , -1.618033988749895 , 0.0 ]
            , [ 1.0 , 1.618033988749895 , 0.0 ]
            ]
      faces2 =  [ [ 2 , 4 , 9 ]
                , [ 1 , 8 , 10 ]
                , [ 1 , 6 , 8 ]
                , [ 4 , 6 , 9 ]
                , [ 8 , 6 , 4 ]
                , [ 9 , 6 , 3 ]
                , [ 1 , 3 , 6 ]
                , [ 11 , 5 , 2 ]
                , [ 11 , 9 , 3 ]
                , [ 2 , 9 , 11 ]
                , [ 8 , 4 , 0 ]
                , [ 0 , 2 , 5 ]
                , [ 4 , 2 , 0 ]
                , [ 0 , 5 , 10 ]
                , [ 10 , 8 , 0 ]
                , [ 7 , 3 , 1 ]
                , [ 10 , 7 , 1 ]
                , [ 5 , 7 , 10 ]
                , [ 11 , 7 , 5 ]
                , [ 3 , 7 , 11 ]
                ]
  polyhedraIntersection (vs1, faces1) (vs2, faces2)
