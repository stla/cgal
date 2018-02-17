module Examples where

fiveTetrahedraVertices :: [[Double]]
fiveTetrahedraVertices = [[ a,  a,  a],
                          [ a,  a, -a],
                          [ a, -a,  a],
                          [-a, -a,  a],
                          [-a,  a, -a],
                          [-a,  a,  a],
                          [ 0,  b, -c],
                          [ 0, -b, -c],
                          [ 0, -b,  c],
                          [ c,  0, -b],
                          [-c,  0, -b],
                          [-c,  0,  b],
                          [ b,  c,  0],
                          [ b, -c,  0],
                          [-b, -c,  0],
                          [-b,  c,  0],
                          [ 0,  b,  c],
                          [ a, -a, -a],
                          [ c,  0,  b],
                          [-a, -a, -a]]
  where
    phi = (1 + sqrt 5) / 2
    a = 1 / sqrt 3
    b = a / phi
    c = a * phi

fiveTetrahedraFaces :: [[[Int]]]
fiveTetrahedraFaces =
  [tetra1Idxs, tetra2Idxs, tetra3Idxs, tetra4Idxs, tetra5Idxs]
  where
    tetra1Idxs = [[16,13, 1],
                  [16,10,13],
                  [10, 1,13],
                  [10,16, 1]]
    tetra2Idxs = [[17, 0, 3],
                  [17, 4, 0],
                  [ 4,17, 3],
                  [ 4, 3, 0]]
    tetra3Idxs = [[18, 5,14],
                  [18, 6, 5],
                  [18,14, 6],
                  [ 6,14, 5]]
    tetra4Idxs = [[ 2,12,11],
                  [ 2, 7,12],
                  [ 7,11,12],
                  [11, 7, 2]]
    tetra5Idxs = [[19,15, 9],
                  [19, 8,15],
                  [ 8, 9,15],
                  [19, 9, 8]]
