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
  , __nvertices :: CUInt
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
  , __faceSizes :: Ptr CUInt -- inutile
  , __nfaces :: CUInt
  , __edges :: Ptr (Ptr CUInt)
  , __nedges :: CUInt
}

instance Storable CMesh where
    sizeOf    __ = #{size MeshT}
    alignment __ = #{alignment MeshT}
    peek ptr = do
      vs <- #{peek MeshT, vertices} ptr
      nvs <- #{peek MeshT, nvertices} ptr
      fs <- #{peek MeshT, faces} ptr
      fss <- #{peek MeshT, faceSizes} ptr
      nf <- #{peek MeshT, nfaces} ptr
      es <- #{peek MeshT, edges} ptr
      ne <- #{peek MeshT, nedges} ptr
      return CMesh { __vertices = vs
                   , __nvertices' = nvs
                   , __faces = fs
                   , __faceSizes = fss
                   , __nfaces = nf
                   , __edges = es
                   , __nedges = ne }
    poke ptr (CMesh r1 r2 r3 r4 r5 r6 r7)
      = do
        #{poke MeshT, vertices} ptr r1
        #{poke MeshT, nvertices} ptr r2
        #{poke MeshT, faces} ptr r3
        #{poke MeshT, faceSizes} ptr r4
        #{poke MeshT, nfaces} ptr r5
        #{poke MeshT, edges} ptr r6
        #{poke MeshT, nedges} ptr r7

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


foreign import ccall unsafe "intersectionTwoPolyhedra" c_polyhedraIntersection
  :: Ptr CDouble -> CSize -> Ptr CInt -> Ptr CInt -> CSize
  -> Ptr CDouble -> CSize -> Ptr CInt -> Ptr CInt -> CSize -> IO (Ptr CMesh)
