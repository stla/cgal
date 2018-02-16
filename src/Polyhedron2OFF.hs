{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Polyhedron2OFF
  where
import           Foreign.C.Types
import           Foreign.Marshal.Alloc      (free, mallocBytes)
import           Foreign.Marshal.Array      (pokeArray)
import           Foreign.Ptr                (Ptr)
import           Foreign.Storable           (sizeOf)

foreign import ccall unsafe "polyhedron2off" c_polyhedron2off
  :: Ptr CDouble -> CSize -> Ptr CInt -> Ptr CInt -> CSize -> IO ()

polyhedron2off :: [[Double]] -> [[Int]] -> IO ()
polyhedron2off vertices faces = do
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
  _ <- c_polyhedron2off verticesPtr (fromIntegral nvertices) facesPtr
                        facesizesPtr (fromIntegral nfaces)
  free verticesPtr
  free facesPtr
  free facesizesPtr

ttest :: IO ()
ttest = do
  let vertices = [ [1, 0, 0]
                 , [0, 1, 0]
                 , [0, 0, 1]
                 , [2, 0, 0]
                 , [0, 2, 0]
                 , [0, 0, 2]
                 , [0, 1, 1]
                 ]
      faces = [ [0, 1, 2]
              , [3, 4, 5, 6]
              ]
  polyhedron2off vertices faces


-- void polyhedron2off(
--   double* vertices,
--   size_t nvertices,
--   int* faces,
--   int* facesizes,
--   size_t nfaces)
