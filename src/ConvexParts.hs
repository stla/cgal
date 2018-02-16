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
