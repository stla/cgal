module PolyhedraIntersection
  where
import           Foreign.Marshal.Alloc (free, mallocBytes)
-- import           Foreign.C.Types
import           Foreign.Marshal.Array (pokeArray)
import           Foreign.Storable      (peek, sizeOf)
import           Helpers               (unmakeMesh, makeCPolyhedra)
import           Mesh
import           Types

polyhedraIntersections :: [([[Double]], [[Int]])] -> IO Mesh
polyhedraIntersections polyhedras = do
  cpolyhedras <- makeCPolyhedra polyhedras
  let npolyhedras = length polyhedras
  cpolyhedrasPtr <- mallocBytes (npolyhedras * sizeOf (undefined :: CPolyhedron))
  pokeArray cpolyhedrasPtr cpolyhedras
  cmeshPtr <- c_intersectionNpolyhedra cpolyhedrasPtr (fromIntegral npolyhedras)
  cmesh <- peek cmeshPtr
  free cmeshPtr;
  cMeshToMesh cmesh
