import qualified Data.IntMap.Strict    as IM
import           PolyhedraIntersection
import           Text.Show.Pretty
import           Types

main :: IO ()
main = do
  mesh <- testCubes
  pPrint mesh
  -- pPrint $ IM.elems (_vertices mesh)
  -- pPrint $ map _verticesIds (_faces mesh)
  -- pPrint $ _edges mesh
