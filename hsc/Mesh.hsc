{-# LANGUAGE ForeignFunctionInterface #-}
module Mesh
  where
import           Control.Monad       ((<$!>), (=<<))
import           Types
import           Foreign

#include "intersection.hpp"

data CVertex = CVertex {
  __point :: Ptr CDouble
}

instance Storable CVertex where
    sizeOf    __ = #{size VertexT}
    alignment __ = #{alignment VertexT}
    peek ptr = do
      point'  <- #{peek VertexT, point} ptr
      return CVertex { __point = point' }
    poke ptr (CVertex r1)
      = do
        #{poke VertexT, point} ptr r1

cVertexToVertex3 :: CVertex -> IO Vertex3
cVertexToVertex3 cvertex = do
  coords <- (<$!>) (map realToFrac) (peekArray 3 (__point cvertex))
  return $ Vertex3 (coords!!0) (coords!!1) (coords!!2)

data CFace = CFace {
  __verticesIds :: Ptr CUInt
  __nvertices :: CUInt
}

instance Storable CFace where
    sizeOf    __ = #{size FaceT}
    alignment __ = #{alignment FaceT}
    peek ptr = do
      ids  <- #{peek FaceT, verticesIds} ptr
      nvs  <- #{peek FaceT, nvertices} ptr
      return CFace { __verticesIds = ids
                   , __nvertices   = nvs }
    poke ptr (CFace r1 r2)
      = do
        #{poke FaceT, verticesIds} ptr r1
        #{poke FaceT, nvertices} ptr r2

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
  , __faceSizes :: Ptr CUInt
  , __nfaces :: CUInt
}

instance Storable CMesh where
    sizeOf    __ = #{size MeshT}
    alignment __ = #{alignment MeshT}
    peek ptr = do
      vs <- #{peek MeshT, verticesIds} ptr
      nvs <- #{peek MeshT, nvertices} ptr
      fs <- #{peek MeshT, faces} ptr
      fss <- #{peek MeshT, faceSizes} ptr
      nf <- #{peek MeshT, nfaces} ptr
      return CMesh { __vertices = vs
                   , __nvertices' = nvs
                   , __faces = fs
                   , __faceSizes = fss
                   , __nfaces = nf }
    poke ptr (CMesh r1 r2 r3 r4 r5)
      = do
        #{poke MeshT, vertices} ptr r1
        #{poke MeshT, nvertices} ptr r2
        #{poke MeshT, faces} ptr r3
        #{poke MeshT, faceSizes} ptr r4
        #{poke MeshT, nfaces} ptr r5

cMeshtoMesh :: CMesh -> IO Mesh
cMeshtoMesh cmesh = do
  let nvertices = fromIntegral $ __nvertices' cmesh
      nfaces = fromIntegral $ __nfaces cmesh
