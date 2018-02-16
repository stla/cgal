module ConvexParts
  where
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (pokeArray, peekArray)
import           Foreign.Storable      (peek, sizeOf)
import           Helpers               (unmakeMesh)
import           Mesh
import           Types

convexParts_ :: ([[Double]],[[Int]]) -> IO [Mesh]
convexParts_ (vertices, faces) = do
  let nvertices = length vertices
      nfaces = length faces
      facesizes = map length faces
      l = sum facesizes
  verticesPtr <- mallocBytes (nvertices * 3 * sizeOf (undefined :: CDouble))
  pokeArray verticesPtr (concatMap (map realToFrac) vertices)
  facesPtr <- mallocBytes (l * sizeOf (undefined :: CInt))
  pokeArray facesPtr (concatMap (map fromIntegral) faces)
  facesizesPtr <- mallocBytes (nfaces * sizeOf (undefined :: CInt))
  pokeArray facesizesPtr (map fromIntegral facesizes)
  nconvexPartsPtr <- mallocBytes (sizeOf (undefined :: CSize))
  meshPtr <- c_convexParts verticesPtr (fromIntegral nvertices) facesPtr
                           facesizesPtr (fromIntegral nfaces) nconvexPartsPtr
  free verticesPtr
  free facesPtr
  free facesizesPtr
  nconvexParts <- peek nconvexPartsPtr
  cmeshes <- peekArray (fromIntegral nconvexParts) meshPtr
  free nconvexPartsPtr
  mapM cMeshToMesh cmeshes

testConvexParts :: IO [Mesh]
testConvexParts = do
  let x = 2.1806973249
  let y = 3.5617820682
  let vertices =  [ [ -x , -x , -x ]    -- 0
                  , [ -y , 0.0 , 0.0 ]  -- 1
                  , [ 0.0 , -y , 0.0 ]  -- 2
                  , [ 0.0 , 0.0 , -y ]  -- 3
                  , [ -x , -x , x ]     -- 4
                  , [ 0.0 , 0.0 , y ]   -- 5
                  , [ -x , x , -x ]     -- 6
                  , [ 0.0 , y , 0.0 ]   -- 7
                  , [ -x , x , x ]      -- 8
                  , [ x , -x , -x ]     -- 9
                  , [ y , 0.0 , 0.0 ]   -- 10
                  , [ x , -x , x ]      -- 11
                  , [ x , x , -x ]      -- 12
                  , [ x , x , x ]       -- 13
                  ]
  let faces = [ [3, 2, 0]
              , [0, 1, 3]
              , [2, 1, 0]
              , [4, 2, 5]
              , [5, 1, 4]
              , [4, 1, 2]
              , [6, 7, 3]
              , [3, 1, 6]
              , [6, 1, 7]
              , [5, 7, 8]
              , [8, 1, 5]
              , [7, 1, 8]
              , [9, 2, 3]
              , [3, 10, 9]
              , [9, 10, 2]
              , [5, 2, 11]
              , [11, 10, 5]
              , [2, 10, 11]
              , [3, 7, 12]
              , [12, 10, 3]
              , [7, 10, 12]
              , [13, 7, 5]
              , [5, 10, 13]
              , [13, 10, 7]
              ]
  convexParts_ (vertices, faces)
