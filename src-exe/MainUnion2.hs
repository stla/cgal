{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
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

dbl2Cdbl :: [[Double]] -> [CDouble]
dbl2Cdbl = concatMap (map realToFrac)
int2Cint :: [[Int]] -> [CInt]
int2Cint = concatMap (map fromIntegral)


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

  -- polyhedronT p1;
  -- p1.vertices = cubeVertices1;
  -- p1.nvertices = 8;
  -- p1.faces = cubeTriFaces1;
  -- p1.facesizes = {3,3,3,3,3,3,3,3,3,3,3,3};
  -- p1.nfaces = 12;
  putStrLn "a"
  vsPtr1 <- mallocBytes(8 * 3 * sizeOf(undefined :: CDouble))
  putStrLn "b"
  pokeArray vsPtr1 (dbl2Cdbl cubeVertices1)
  putStrLn "c"
  fsPtr1 <- mallocBytes(12 * 3 * sizeOf(undefined :: CInt))
  putStrLn "d"
  pokeArray fsPtr1 (int2Cint cubeTriFaces1)
  putStrLn "e"
  fssPtr1 <- mallocBytes(12 * sizeOf(undefined :: CInt))
  putStrLn "f"
  pokeArray fssPtr1 [3,3,3,3,3,3,3,3,3,3,3,3]
  putStrLn "build p1\n"
  let p1 = CPolyhedron
            {
              __vertices' = vsPtr1
            , __nvertices'' = 8
            , __faces' = fsPtr1
            , __facesizes' = fssPtr1
            , __nfaces' = 12
            }
  vsPtr2 <- mallocBytes(8 * 3 *sizeOf(undefined :: CDouble))
  pokeArray vsPtr2 (dbl2Cdbl cubeVertices2)
  fsPtr2 <- mallocBytes(12 * 3* sizeOf(undefined :: CInt))
  pokeArray fsPtr2 (int2Cint cubeTriFaces2)
  fssPtr2 <- mallocBytes(12 * sizeOf(undefined :: CInt))
  pokeArray fssPtr2 [3,3,3,3,3,3,3,3,3,3,3,3]
  putStrLn "build p2\n"
  let p2 = CPolyhedron
            {
              __vertices' = vsPtr2
            , __nvertices'' = 8
            , __faces' = fsPtr2
            , __facesizes' = fssPtr2
            , __nfaces' = 12
            }
  vsPtr3 <- mallocBytes(8 * 3 * sizeOf(undefined :: CDouble))
  pokeArray vsPtr3 (dbl2Cdbl cubeVertices3)
  fsPtr3 <- mallocBytes(12 * 3 *sizeOf(undefined :: CInt))
  pokeArray fsPtr3 (int2Cint cubeTriFaces3)
  fssPtr3 <- mallocBytes(12 * sizeOf(undefined :: CInt))
  pokeArray fssPtr3 [3,3,3,3,3,3,3,3,3,3,3,3]
  putStrLn "build p3\n"
  let p3 = CPolyhedron
            {
              __vertices' = vsPtr3
            , __nvertices'' = 8
            , __faces' = fsPtr3
            , __facesizes' = fssPtr3
            , __nfaces' = 12
            }
  p1p2p3Ptr <- mallocBytes(3 * sizeOf (undefined :: CPolyhedron))
  pokeArray p1p2p3Ptr [p1,p2,p3]
  unionPtr <- c_unionNpolyhedra p1p2p3Ptr 3
  union <- peek unionPtr
  mesh <- cMeshToMesh union
  pPrint mesh

  -- --  ### CECI MARCHE ### --
  -- union <- polyhedraUnion_ (cubeVertices3, cubeTriFaces3) (cubeVertices2, cubeTriFaces2) (cubeVertices1, cubeTriFaces1)
  -- -- -- let (vs, fs) = unmakeMesh union
  -- -- -- union2 <- polyhedraUnion_ (vs,fs) (cubeVertices1, cubeTriFaces1)
  -- pPrint union
  -- -- ###             ### --
