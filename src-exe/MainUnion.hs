import qualified Data.IntMap.Strict as IM
import           Data.Maybe
import           Examples
import           PolyhedraUnion
import           System.IO
import           Text.Show.Pretty
import           Data.List
import           Control.Monad         (forM_)
import           Foreign               (Ptr, alloca, new, malloc)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (peekArray, pokeArray, newArray)
import           Foreign.Storable      (peek, poke, sizeOf)
import           Helpers               (unmakeMesh)
import           Mesh
import           Types

approx :: RealFrac a => Int -> a -> a
approx n x = fromInteger (round $ x * (10^n)) / (10.0^^n)

toDbl (Vertex3 x y z) = [x,y,z]

main :: IO ()
main = do

  -- vsPtr1 <- malloc
  -- pokeArray vsPtr1 (concatMap (map realToFrac) cubeVertices1 :: [CDouble])
  -- fsPtr1 <- malloc
  -- pokeArray fsPtr1 (concatMap (map fromIntegral) cubeTriFaces1 :: [CInt])
  -- fssPtr1 <- malloc
  -- pokeArray fssPtr1 [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 :: CInt]
  -- vsPtr2 <- malloc
  -- pokeArray vsPtr2 (concatMap (map realToFrac) cubeVertices2 :: [CDouble])
  -- fsPtr2 <- malloc
  -- pokeArray fsPtr2 (concatMap (map fromIntegral) cubeTriFaces2 :: [CInt])
  -- fssPtr2 <- malloc
  -- pokeArray fssPtr2 [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 :: CInt]
  -- vsPtr3 <- malloc
  -- pokeArray vsPtr3 (concatMap (map realToFrac) cubeVertices3 :: [CDouble])
  -- fsPtr3 <- malloc
  -- pokeArray fsPtr3 (concatMap (map fromIntegral) cubeTriFaces3 :: [CInt])
  -- fssPtr3 <- malloc
  -- pokeArray fssPtr3 [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 :: CInt]
  -- let p1 = CPolyhedron {
  --     __vertices' = vsPtr1
  --   , __nvertices'' = 8
  --   , __faces' = fsPtr1
  --   , __facesizes' = fssPtr1
  --   , __nfaces' = 12    }
  -- let p2 = CPolyhedron {
  --     __vertices' = vsPtr2
  --   , __nvertices'' = 8
  --   , __faces' = fsPtr2
  --   , __facesizes' = fssPtr2
  --   , __nfaces' = 12   }
  -- let p3 = CPolyhedron {
  --     __vertices' = vsPtr3
  --   , __nvertices'' = 8
  --   , __faces' = fsPtr3
  --   , __facesizes' = fssPtr3
  --   , __nfaces' = 12    }
  -- polyhPtr <- mallocBytes (3 * sizeOf (undefined :: CPolyhedron))
  -- pokeArray polyhPtr [p1, p2, p3]
  -- union <- c_unionNpolyhedra polyhPtr 3
  -- u <- peek union
  -- print u

  --  ### CECI MARCHE ### --
  union <- polyhedraUnion_ (cubeVertices3, cubeTriFaces3) (cubeVertices2, cubeTriFaces2) (cubeVertices1, cubeTriFaces1)
  -- -- let (vs, fs) = unmakeMesh union
  -- -- union2 <- polyhedraUnion_ (vs,fs) (cubeVertices1, cubeTriFaces1)
  pPrint union
  -- ###             ### --

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
