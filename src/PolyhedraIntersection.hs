module PolyhedraIntersection
  where
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (pokeArray)
import           Foreign.Storable      (peek, sizeOf)
import           Helpers               
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
  free cpolyhedrasPtr
  cMeshToMesh cmesh
