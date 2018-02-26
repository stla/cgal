{-# LINE 1 "Mesh.hsc" #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mesh
  ( cMeshToMesh
  , c_convexParts
  , c_unionNpolyhedra
  , c_intersectionNpolyhedra
  , CPolyhedron (..))
  where
import           Control.Monad      ((<$!>), (=<<))
import qualified Data.IntMap.Strict as IM
import           Foreign
import           Foreign.C.Types
import           Types

data CPolyhedron = CPolyhedron {
    __vertices'   :: Ptr CDouble
  , __nvertices'' :: CSize
  , __faces'      :: Ptr CInt
  , __facesizes'  :: Ptr CInt
  , __nfaces'     :: CSize
}

instance Storable CPolyhedron  where
    sizeOf    __ = (40)
{-# LINE 24 "Mesh.hsc" #-}
    alignment __ = 8
{-# LINE 25 "Mesh.hsc" #-}
    peek ptr = do
      vertices'  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 27 "Mesh.hsc" #-}
      nvertices'  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 28 "Mesh.hsc" #-}
      faces'  <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 29 "Mesh.hsc" #-}
      facesizes'  <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 30 "Mesh.hsc" #-}
      nfaces'  <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 31 "Mesh.hsc" #-}
      return CPolyhedron  {  __vertices' = vertices'
                           , __nvertices'' = nvertices'
                           , __faces' = faces'
                           , __facesizes' = facesizes'
                           , __nfaces' = nfaces' }
    poke ptr (CPolyhedron r1 r2 r3 r4 r5)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 39 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 40 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
{-# LINE 41 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
{-# LINE 42 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr r5
{-# LINE 43 "Mesh.hsc" #-}


data CVertex = CVertex {
  __point :: Ptr CDouble
}

instance Storable CVertex where
    sizeOf    __ = (8)
{-# LINE 51 "Mesh.hsc" #-}
    alignment __ = 8
{-# LINE 52 "Mesh.hsc" #-}
    peek ptr = do
      point'  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 54 "Mesh.hsc" #-}
      return CVertex { __point = point' }
    poke ptr (CVertex r1)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 58 "Mesh.hsc" #-}

cVertexToVertex3 :: CVertex -> IO Vertex3
cVertexToVertex3 cvertex = do
  coords <- (<$!>) (map realToFrac) (peekArray 3 (__point cvertex))
  return $ Vertex3 (coords!!0) (coords!!1) (coords!!2)

data CFace = CFace {
    __verticesIds :: Ptr CUInt
  , __nvertices   :: CUInt
}

instance Storable CFace where
    sizeOf    __ = (16)
{-# LINE 71 "Mesh.hsc" #-}
    alignment __ = 8
{-# LINE 72 "Mesh.hsc" #-}
    peek ptr = do
      ids  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 74 "Mesh.hsc" #-}
      nvs  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 75 "Mesh.hsc" #-}
      return CFace { __verticesIds = ids
                   , __nvertices   = nvs }
    poke ptr (CFace r1 r2)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 80 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 81 "Mesh.hsc" #-}

cFaceToFace :: CFace -> IO Face
cFaceToFace cface = do
  let nvertices = fromIntegral $ __nvertices cface
  verticesIds <- (<$!>) (map fromIntegral)
                        (peekArray nvertices (__verticesIds cface))
  return $ Face { _verticesIds = verticesIds }

data CMesh = CMesh {
    __vertices   :: Ptr CVertex
  , __nvertices' :: CUInt
  , __faces      :: Ptr CFace
  , __faceSizes  :: Ptr CUInt -- inutile
  , __nfaces     :: CUInt
  , __edges      :: Ptr (Ptr CUInt)
  , __nedges     :: CUInt
} deriving Show

instance Storable CMesh where
    sizeOf    __ = (56)
{-# LINE 101 "Mesh.hsc" #-}
    alignment __ = 8
{-# LINE 102 "Mesh.hsc" #-}
    peek ptr = do
      vs <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 104 "Mesh.hsc" #-}
      nvs <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 105 "Mesh.hsc" #-}
      fs <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 106 "Mesh.hsc" #-}
      fss <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 107 "Mesh.hsc" #-}
      nf <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 108 "Mesh.hsc" #-}
      es <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 109 "Mesh.hsc" #-}
      ne <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 110 "Mesh.hsc" #-}
      return CMesh { __vertices = vs
                   , __nvertices' = nvs
                   , __faces = fs
                   , __faceSizes = fss
                   , __nfaces = nf
                   , __edges = es
                   , __nedges = ne }
    poke ptr (CMesh r1 r2 r3 r4 r5 r6 r7)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 120 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 121 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
{-# LINE 122 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
{-# LINE 123 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr r5
{-# LINE 124 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr r6
{-# LINE 125 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 48) ptr r7
{-# LINE 126 "Mesh.hsc" #-}

cMeshToMesh :: CMesh -> IO Mesh
cMeshToMesh cmesh = do
  let nvertices = fromIntegral $ __nvertices' cmesh
      nfaces = fromIntegral $ __nfaces cmesh
      nedges = fromIntegral $ __nedges cmesh
  vertices <- peekArray nvertices (__vertices cmesh)
  faces <- peekArray nfaces (__faces cmesh)
  vertices' <- mapM cVertexToVertex3 vertices
  faces' <- mapM cFaceToFace faces
  edges <- (<$!>) (map (\x -> Pair (fromIntegral (x!!0)) (fromIntegral (x!!1))))
                  ((=<<) (mapM (peekArray 2))
                         (peekArray nedges (__edges cmesh)))
  return $ Mesh { _vertices = IM.fromAscList (zip [0 .. nvertices-1] vertices')
                , _faces = faces'
                , _edges = Just edges}


-- foreign import ccall unsafe "intersectionTwoPolyhedra" c_polyhedraIntersection
--   :: Ptr CDouble -> CSize -> Ptr CInt -> Ptr CInt -> CSize
--   -> Ptr CDouble -> CSize -> Ptr CInt -> Ptr CInt -> CSize -> IO (Ptr CMesh)
--
-- foreign import ccall unsafe "unionThreePolyhedra" c_polyhedraUnion
--   :: Ptr CDouble -> CSize -> Ptr CInt -> Ptr CInt -> CSize
--   -> Ptr CDouble -> CSize -> Ptr CInt -> Ptr CInt -> CSize
--   -> Ptr CDouble -> CSize -> Ptr CInt -> Ptr CInt -> CSize -> IO (Ptr CMesh)

foreign import ccall unsafe "convexParts" c_convexParts
  :: Ptr CDouble -> CSize -> Ptr CInt -> Ptr CInt -> CSize -> Ptr CSize
  -> IO (Ptr CMesh)

foreign import ccall unsafe "unionNPolyhedra" c_unionNpolyhedra
  :: Ptr CPolyhedron -> CUInt -> IO (Ptr CMesh)

foreign import ccall unsafe "intersectionNPolyhedra" c_intersectionNpolyhedra
  :: Ptr CPolyhedron -> CUInt -> IO (Ptr CMesh)
