{-# LINE 1 "Mesh.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Mesh
  (c_polyhedraIntersection, cMeshToMesh)
  where
import           Control.Monad       ((<$!>))
import           Types
import           Foreign
import           Foreign.C.Types



data CVertex = CVertex {
  __point :: Ptr CDouble
}

instance Storable CVertex where
    sizeOf    __ = (8)
{-# LINE 19 "Mesh.hsc" #-}
    alignment __ = 8
{-# LINE 20 "Mesh.hsc" #-}
    peek ptr = do
      point'  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 22 "Mesh.hsc" #-}
      return CVertex { __point = point' }
    poke ptr (CVertex r1)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 26 "Mesh.hsc" #-}

cVertexToVertex3 :: CVertex -> IO Vertex3
cVertexToVertex3 cvertex = do
  coords <- (<$!>) (map realToFrac) (peekArray 3 (__point cvertex))
  return $ Vertex3 (coords!!0) (coords!!1) (coords!!2)

data CFace = CFace {
    __verticesIds :: Ptr CUInt
  , __nvertices :: CUInt
}

instance Storable CFace where
    sizeOf    __ = (16)
{-# LINE 39 "Mesh.hsc" #-}
    alignment __ = 8
{-# LINE 40 "Mesh.hsc" #-}
    peek ptr = do
      ids  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 42 "Mesh.hsc" #-}
      nvs  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 43 "Mesh.hsc" #-}
      return CFace { __verticesIds = ids
                   , __nvertices   = nvs }
    poke ptr (CFace r1 r2)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 48 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 49 "Mesh.hsc" #-}

cFaceToFace :: CFace -> IO Face
cFaceToFace cface = do
  let nvertices = fromIntegral $ __nvertices cface
  verticesIds <- (<$!>) (map fromIntegral)
                        (peekArray nvertices (__verticesIds cface))
  return $ Face { _verticesIds = verticesIds }

data CMesh = CMesh {
    __vertices :: Ptr CVertex
  , __nvertices' :: CUInt
  , __faces :: Ptr CFace
  , __faceSizes :: Ptr CUInt -- inutile
  , __nfaces :: CUInt
}

instance Storable CMesh where
    sizeOf    __ = (40)
{-# LINE 67 "Mesh.hsc" #-}
    alignment __ = 8
{-# LINE 68 "Mesh.hsc" #-}
    peek ptr = do
      vs <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 70 "Mesh.hsc" #-}
      nvs <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 71 "Mesh.hsc" #-}
      fs <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 72 "Mesh.hsc" #-}
      fss <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 73 "Mesh.hsc" #-}
      nf <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 74 "Mesh.hsc" #-}
      return CMesh { __vertices = vs
                   , __nvertices' = nvs
                   , __faces = fs
                   , __faceSizes = fss
                   , __nfaces = nf }
    poke ptr (CMesh r1 r2 r3 r4 r5)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 82 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 83 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
{-# LINE 84 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
{-# LINE 85 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr r5
{-# LINE 86 "Mesh.hsc" #-}

cMeshToMesh :: CMesh -> IO Mesh
cMeshToMesh cmesh = do
  let nvertices = fromIntegral $ __nvertices' cmesh
      nfaces = fromIntegral $ __nfaces cmesh
  vertices <- peekArray nvertices (__vertices cmesh)
  faces <- peekArray nfaces (__faces cmesh)
  vertices' <- mapM cVertexToVertex3 vertices
  faces' <- mapM cFaceToFace faces
  return $ Mesh { _vertices = vertices'
                , _faces = faces' }

foreign import ccall unsafe "intersectionTwoPolyhedra" c_polyhedraIntersection
  :: Ptr CDouble -> CSize -> Ptr CInt -> Ptr CInt -> CSize
  -> Ptr CDouble -> CSize -> Ptr CInt -> Ptr CInt -> CSize -> IO (Ptr CMesh)
