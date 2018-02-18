import qualified Data.IntMap.Strict as IM
import           Examples
import           Helpers
import           PolyhedraUnion
import           System.IO
import           Text.Show.Pretty
import           Types

approx :: RealFrac a => Int -> a -> a
approx n x = fromInteger (round $ x * (10^n)) / (10.0^^n)

main :: IO ()
main = do
  let tetra1 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!0)
      tetra2 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!1)
      tetra3 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!2)
      tetra4 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!3)
      tetra5 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!4)
  union1 <- polyhedraUnion (roundVertices 6 tetra1) (roundVertices 6 tetra2)
  union2 <- polyhedraUnion (roundVertices 6 union1) (roundVertices 6 tetra3)
  pPrint union2

  -- putStrLn "vertices:"
  -- pPrint $ IM.elems (_vertices mesh)
  -- putStrLn "faces:"
  -- pPrint $ map _verticesIds (_faces mesh)
  -- putStrLn "edges:"
  -- pPrint $ _edges mesh
