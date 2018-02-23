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
-- let cube1 = makeMesh $ fixIndices allVertices (allFaces!!0)
--     cube2 = makeMesh $ fixIndices allVertices (allFaces!!1)
--     cube3 = makeMesh $ fixIndices allVertices (allFaces!!2)
--     cube4 = makeMesh $ fixIndices allVertices (allFaces!!3)
--     cube5 = makeMesh $ fixIndices allVertices (allFaces!!4)
--
-- union1 <- polyhedraUnion cube1 cube2
-- union2 <- polyhedraUnion union1 cube3
-- -- inter3 <- polyhedraIntersection inter2 cube4
-- -- inter4 <- polyhedraIntersection inter3 cube5
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


tetra1 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!0)
tetra2 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!1)
tetra3 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!2)
tetra4 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!3)
tetra5 = makeMesh $ fixIndices fiveTetrahedraVertices (fiveTetrahedraFaces!!4)
vertices1 = map toDbl $ IM.elems (_vertices tetra1)
vertices2 = map toDbl $ IM.elems (_vertices tetra2)
vertices = vertices1 `union` vertices2
(vs, faces) = fixIndices fiveTetrahedraVertices ((fiveTetrahedraFaces!!0) `union` (fiveTetrahedraFaces!!1))
union <- polyhedraUnion_ (vs,faces) (vs,faces)
pPrint union

-- pPrint vertices
-- let tetra1bis = (vertices, fiveTetrahedraFaces!!0)
-- let tetra2bis = (vertices, fiveTetrahedraFaces!!1)
-- union1 <- polyhedraUnion_ tetra1bis tetra2bis
-- putStrLn "done"
-- union1 <- polyhedraUnion (roundVertices 6 tetra1) (roundVertices 6 tetra5)
-- union2 <- polyhedraUnion (roundVertices 6 union1) (roundVertices 6 tetra3)
-- pPrint union2

-- putStrLn "vertices:"
-- pPrint $ IM.elems (_vertices mesh)
-- putStrLn "faces:"
-- pPrint $ map _verticesIds (_faces mesh)
-- putStrLn "edges:"
-- pPrint $ _edges mesh
