typedef struct Vertex {
  double* point;
} VertexT;

typedef struct Face {
  unsigned* verticesIds;
  unsigned nvertices;
} FaceT;

typedef struct Mesh {
  VertexT* vertices;
  unsigned nvertices;
  FaceT* faces;
  unsigned* faceSizes;
  unsigned nfaces;
} MeshT;
