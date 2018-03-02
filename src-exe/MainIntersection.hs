import qualified Data.IntMap.Strict    as IM
import           PolyhedraIntersection
import           Text.Show.Pretty
import           Types
import           Examples
import           System.IO
import           Helpers
import Polyhedron2OFF
import Data.List
import Data.Maybe

approx :: RealFrac a => Int -> a -> a
approx n x = fromInteger (round $ x * (10^n)) / (10.0^^n)

fromPair (Pair i j) = (i,j)

main :: IO ()
main = do

  -- -- Reuleux tetrahedron -- --
  mesh <- polyhedraIntersections [(sphere1randomCenterA, sphere1facet),
                                  (sphere1randomCenterB, sphere1facet),
                                  (sphere1randomCenterC, sphere1facet),
                                  (sphere1randomCenterD, sphere1facet),
                                  (regularTetrahedron, regularTetrahedronFacets)]


  -- mesh <- polyhedraIntersections [sphere1', sphere2']
  pPrint mesh
  putStrLn "vertices:"
  pPrint $ IM.elems (_vertices mesh)
  putStrLn "faces:"
  pPrint $ map _verticesIds (_faces mesh)
  putStrLn "edges:"
  pPrint $ map fromPair $ fromJust $ _edges mesh



  -- -- -- five cubes -- --
  -- let x = (1 + sqrt 5) / 2
  -- let y = x - 1
  -- let allVertices = [ [-1, 1, 1], -- 0
  --                     [0, y, x],  -- 1
  --                     [y, x, 0], -- 2
  --                     [-y, x, 0], -- 3
  --                     [0, y, -x], -- 4
  --                     [0, -y, x], -- 5
  --                     [1, -1, 1], -- 6
  --                     [x, 0, y], -- 7
  --                     [1, 1, 1], -- 8
  --                     [1, 1, -1], -- 9
  --                     [x, 0, -y], -- 10
  --                     [y, -x, 0], -- 11
  --                     [1, -1, -1], -- 12
  --                     [-1, -1, 1], -- 13
  --                     [-x, 0, y], -- 14
  --                     [-1, 1, -1], -- 15
  --                     [-x, 0, -y], -- 16
  --                     [-1, -1, -1], -- 17
  --                     [-y, -x, 0], -- 18
  --                     [0, -y, -x] ] -- 19
  -- let allFaces = [[ [3,1,7,9],
  --                   [9,19,16,3],
  --                   [1,3,16,13],
  --                   [16,19,11,13],
  --                   [1,13,11,7],
  --                   [9,7,11,19] ],
  --                 [ [2,4,16,0],
  --                   [2,7,12,4],
  --                   [2,0,5,7],
  --                   [4,12,18,16],
  --                   [0,16,18,5],
  --                   [7,5,18,12] ],
  --                 [ [3,8,10,4],
  --                   [3,14,5,8],
  --                   [8,5,11,10],
  --                   [10,11,17,4],
  --                   [4,17,14,3],
  --                   [14,17,11,5] ],
  --                 [ [9,12,17,15], -- [x,x,-1]
  --                   [13,17,12,6], -- [x,-1,x]
  --                   [8,6,12,9],   -- [1,x,x]
  --                   [15,17,13,0], -- [-1,x,x]
  --                   [15,0,8,9],   -- [x,1,x]
  --                   [0,13,6,8] ], -- [x,x,1]
  --                 [ [2,10,19,15],
  --                   [15,19,18,14],
  --                   [15,14,1,2],
  --                   [14,18,6,1],
  --                   [1,6,10,2],
  --                   [10,6,18,19] ]
  --               ]
  -- let cube1 = fixIndices allVertices (allFaces!!0)
  --     cube2 = fixIndices allVertices (allFaces!!1)
  --     cube3 = fixIndices allVertices (allFaces!!2)
  --     cube4 = fixIndices allVertices (allFaces!!3)
  --     cube5 = fixIndices allVertices (allFaces!!4)
  -- mesh <- polyhedraIntersections [cube1, cube2, cube3, cube4, cube5]
  -- -- inter2 <- polyhedraIntersection inter1 cube3
  -- -- inter3 <- polyhedraIntersection inter2 cube4
  -- -- inter4 <- polyhedraIntersection inter3 cube5
  -- pPrint mesh
  --
  -- putStrLn "vertices:"
  -- pPrint $ IM.elems (_vertices mesh)
  -- putStrLn "faces:"
  -- pPrint $ map _verticesIds (_faces mesh)
  -- putStrLn "edges:"
  -- pPrint $ map fromPair $ fromJust $ _edges mesh


  -- -- -- intersection cube isocahedron -- --
  -- let vs1 = [ [-1,-1,-1],
  --             [-1,-1, 1],
  --             [-1, 1,-1],
  --             [-1, 1, 1],
  --             [ 1,-1,-1],
  --             [ 1,-1, 1],
  --             [ 1, 1,-1],
  --             [ 1, 1, 1] ]
  --     faces1 =  [ [ 0 , 2 , 6 , 4 ] -- [x,x,-1]
  --               , [ 4 , 5 , 1 , 0 ] -- [x,-1,x]
  --               , [ 6 , 7 , 5 , 4 ] -- [1,x,x]
  --               , [ 0 , 1 , 3 , 2 ] -- [-1,x,x]
  --               , [ 2 , 3 , 7 , 6 ] -- [x,1,x]
  --               , [ 5 , 7 , 3 , 1 ]  -- [x,x,1]
  --               ]
  --     vs2 = [ [ 0.0 , -1.0 , -1.618033988749895 ]
  --           , [ 0.0 , -1.0 , 1.618033988749895 ]
  --           , [ 0.0 , 1.0 , -1.618033988749895 ]
  --           , [ 0.0 , 1.0 , 1.618033988749895 ]
  --           , [ -1.618033988749895 , 0.0 , -1.0 ]
  --           , [ 1.618033988749895 , 0.0 , -1.0 ]
  --           , [ -1.618033988749895 , 0.0 , 1.0 ]
  --           , [ 1.618033988749895 , 0.0 , 1.0 ]
  --           , [ -1.0 , -1.618033988749895 , 0.0 ]
  --           , [ -1.0 , 1.618033988749895 , 0.0 ]
  --           , [ 1.0 , -1.618033988749895 , 0.0 ]
  --           , [ 1.0 , 1.618033988749895 , 0.0 ]
  --           ]
  --     faces2 =  [ [ 2 , 4 , 9 ]
  --               , [ 1 , 8 , 10 ]
  --               , [ 1 , 6 , 8 ]
  --               , [ 4 , 6 , 9 ]
  --               , [ 8 , 6 , 4 ]
  --               , [ 9 , 6 , 3 ]
  --               , [ 1 , 3 , 6 ]
  --               , [ 11 , 5 , 2 ]
  --               , [ 11 , 9 , 3 ]
  --               , [ 2 , 9 , 11 ]
  --               , [ 8 , 4 , 0 ]
  --               , [ 0 , 2 , 5 ]
  --               , [ 4 , 2 , 0 ]
  --               , [ 0 , 5 , 10 ]
  --               , [ 10 , 8 , 0 ]
  --               , [ 7 , 3 , 1 ]
  --               , [ 10 , 7 , 1 ]
  --               , [ 5 , 7 , 10 ]
  --               , [ 11 , 7 , 5 ]
  --               , [ 3 , 7 , 11 ]
  --               ]
  -- mesh <- polyhedraIntersection_ (vs1, faces1) (vs2, faces2)
  -- pPrint mesh

  -- unitSphere <- readFile "Data/sphere.txt"
  -- let sphere = read unitSphere :: ([[Double]],[[Int]])
  --     vertices = map (map (approx 6)) (fst sphere)
  --     sphere' = (vertices, snd sphere)
  --     shiftedVertices = map (map (+0.5)) vertices
  --     sphere'' = (shiftedVertices, snd sphere)
  -- -- polyhedron2off shiftedVertices (snd sphere) "sphere2.off"
  -- mesh <- polyhedraIntersection_ sphere' sphere''
  -- putStrLn "vertices:"
  -- pPrint $ IM.elems (_vertices mesh)
  -- putStrLn "faces:"
  -- pPrint $ map _verticesIds (_faces mesh)
  -- putStrLn "nedges:"
  -- pPrint $ length $ fromJust $ _edges mesh

  -- mesh <- testCubes
  -- pPrint mesh
  -- pPrint $ IM.elems (_vertices mesh)
  -- pPrint $ map _verticesIds (_faces mesh)
  -- pPrint $ _edges mesh

  -- -- -- intersection great stellated dodecahedron cube -- --
  -- let cube1 = fixIndices allVertices (allFaces!!0)
  -- let gsd = (gsdVertices, gsdFaces)
  -- mesh <- polyhedraIntersections [cube1, gsd]
  -- pPrint mesh
  --
  -- putStrLn "vertices:"
  -- pPrint $ IM.elems (_vertices mesh)
  -- putStrLn "faces:"
  -- pPrint $ map _verticesIds (_faces mesh)
  -- putStrLn "edges:"
  -- pPrint $ map fromPair $ fromJust $ _edges mesh
