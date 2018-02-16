import           ConvexParts
import qualified Data.IntMap.Strict    as IM
import           PolyhedraIntersection
import           Text.Show.Pretty
import           Types

-- "mesh" = polyhedron...

main :: IO ()
main = do
  meshes <- testConvexParts
  pPrint meshes
  -- pPrint $ IM.elems (_vertices mesh)
  -- pPrint $ map _verticesIds (_faces mesh)
  -- pPrint $ _edges mesh
