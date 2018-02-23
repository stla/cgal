import qualified Data.IntMap.Strict as IM
import           Data.Maybe
import           Examples
import           Helpers
import           PolyhedraUnion
import           System.IO
import           Text.Show.Pretty
import           Types
import Data.List

approx :: RealFrac a => Int -> a -> a
approx n x = fromInteger (round $ x * (10^n)) / (10.0^^n)

toDbl (Vertex3 x y z) = [x,y,z]

main :: IO ()
main = do

  union <- polyhedraUnion_ (cubeVertices1, cubeTriFaces1) (cubeVertices2, cubeTriFaces2)
  let (vs, fs) = unmakeMesh union
  union2 <- polyhedraUnion_ (vs,fs) (cubeVertices3, cubeTriFaces3)
  pPrint union2
  -- union <- polyhedraUnion_ (cubes''!!0) (cubes''!!1)
  -- pPrint union
  -- let (vs, fs) = unmakeMesh union
  --     union' = fixIndices vs fs
  -- union2 <- polyhedraUnion_ union' (cubes''!!2)
  -- pPrint union2

  -- union <- polyhedraUnion_ (cubes'!!0) (cubes'!!1)
  -- pPrint union
  -- let (vs, fs) = unmakeMesh union
  --     union'' = fixIndices vs fs
  -- union2 <- polyhedraUnion_ union'' (cubes'!!2)
  -- pPrint union2

  -- let (vs3, faces3) = fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!2)
  --     (vsunion, facesunion) = unmakeMesh union
  --     (vsunion', facesunion') = fixIndices fiveTetrahedraVertices facesunion
  -- union2 <- polyhedraUnion_ (vsunion', facesunion') (vs3, faces3)
  -- pPrint union2


  -- unitSphere <- readFile "Data/sphere.txt"
  -- let sphere = read unitSphere :: ([[Double]],[[Int]])
  --     vertices = map (map (approx 6)) (fst sphere)
  --     sphere' = (vertices, snd sphere)
  --     shiftedVertices = map (map (+0.5)) vertices
  --     sphere'' = (shiftedVertices, snd sphere)
  -- mesh <- polyhedraUnion_ sphere' sphere''
  -- putStrLn "vertices:"
  -- pPrint $ IM.elems (_vertices mesh)
  -- putStrLn "faces:"
  -- pPrint $ map _verticesIds (_faces mesh)
  -- putStrLn "nedges:"
  -- pPrint $ length $ fromJust $ _edges mesh


  -- -- -- five Tetrahedra -- --
  -- let tetra1 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!0)
  --     tetra2 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!1)
  --     tetra3 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!2)
  --     tetra4 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!3)
  --     tetra5 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!4)
  --
  -- let vertices1 = map toDbl $ IM.elems (_vertices tetra1)
  --     vertices2 = map toDbl $ IM.elems (_vertices tetra2)
  --     vertices = vertices1 `union` vertices2
  --     (vs1, faces1) = fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!0)
  --     (vs2, faces2) = fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!1)
  --
  -- union <- polyhedraUnion_ (vs1,faces1) (vs2,faces2)
  -- pPrint union
  --
  -- let (vs3, faces3) = fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!2)
  --     (vsunion, facesunion) = unmakeMesh union
  --     (vsunion', facesunion') = fixIndices fiveTetrahedraVertices facesunion
  -- union2 <- polyhedraUnion_ (vsunion', facesunion') (vs3, faces3)
  -- pPrint union2
  -- -- pPrint vertices
  -- -- let tetra1bis = (vertices, fiveTetrahedraFaces!!0)
  -- -- let tetra2bis = (vertices, fiveTetrahedraFaces!!1)
  -- -- union1 <- polyhedraUnion_ tetra1bis tetra2bis
  -- -- putStrLn "done"
  -- -- union1 <- polyhedraUnion (roundVertices 6 tetra1) (roundVertices 6 tetra5)
  -- -- union2 <- polyhedraUnion (roundVertices 6 union1) (roundVertices 6 tetra3)
  -- -- pPrint union2
  --
  -- -- putStrLn "vertices:"
  -- -- pPrint $ IM.elems (_vertices mesh)
  -- -- putStrLn "faces:"
  -- -- pPrint $ map _verticesIds (_faces mesh)
  -- -- putStrLn "edges:"
  -- -- pPrint $ _edges mesh
