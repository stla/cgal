{-# LINE 1 "Mesh.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Mesh
  (c_polyhedraIntersection, cMeshToMesh)
  where
import           Control.Monad       ((<$!>), (=<<))
import           Types
import           Foreign
import           Foreign.C.Types
import qualified Data.IntMap.Strict as IM



data CVertex = CVertex {
  __point :: Ptr CDouble
}

instance Storable CVertex where
    sizeOf    __ = (8)
{-# LINE 20 "Mesh.hsc" #-}
    alignment __ = 8
{-# LINE 21 "Mesh.hsc" #-}
    peek ptr = do
      point'  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 23 "Mesh.hsc" #-}
      return CVertex { __point = point' }
    poke ptr (CVertex r1)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 27 "Mesh.hsc" #-}

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
{-# LINE 40 "Mesh.hsc" #-}
    alignment __ = 8
{-# LINE 41 "Mesh.hsc" #-}
    peek ptr = do
      ids  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 43 "Mesh.hsc" #-}
      nvs  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 44 "Mesh.hsc" #-}
      return CFace { __verticesIds = ids
                   , __nvertices   = nvs }
    poke ptr (CFace r1 r2)
      = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 49 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 50 "Mesh.hsc" #-}

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
  , __edges :: Ptr (Ptr CUInt)
  , __nedges :: CUInt
}

instance Storable CMesh where
    sizeOf    __ = (56)
{-# LINE 70 "Mesh.hsc" #-}
    alignment __ = 8
{-# LINE 71 "Mesh.hsc" #-}
    peek ptr = do
      vs <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 73 "Mesh.hsc" #-}
      nvs <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 74 "Mesh.hsc" #-}
      fs <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 75 "Mesh.hsc" #-}
      fss <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 76 "Mesh.hsc" #-}
      nf <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 77 "Mesh.hsc" #-}
      es <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 78 "Mesh.hsc" #-}
      ne <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 79 "Mesh.hsc" #-}
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
{-# LINE 89 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 90 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
{-# LINE 91 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
{-# LINE 92 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr r5
{-# LINE 93 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr r6
{-# LINE 94 "Mesh.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 48) ptr r7
{-# LINE 95 "Mesh.hsc" #-}

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
                , _edges = edges}
                

foreign import ccall unsafe "intersectionTwoPolyhedra" c_polyhedraIntersection
  :: Ptr CDouble -> CSize -> Ptr CInt -> Ptr CInt -> CSize
  -> Ptr CDouble -> CSize -> Ptr CInt -> Ptr CInt -> CSize -> IO (Ptr CMesh)
