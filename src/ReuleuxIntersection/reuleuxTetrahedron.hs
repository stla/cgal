module ReuleuxTetrahedron where
reuleuxTetrahedron :: [[Double]]
reuleuxTetrahedron =
  [ [0.5 / sqrt 3, -0.5, 0.5 / sqrt 6]
  , [sqrt 3 / 3, 0, -0.5 / sqrt 6]
  , [0.5 / sqrt 3, 0.5, -0.5 / sqrt 6]
  , [0, 0, 0.5 * sqrt 3 / sqrt 2] ]
