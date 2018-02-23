typedef struct polyhedron {
  double* vertices;
  size_t nvertices;
  int* faces;
  int* facesizes;
  size_t nfaces;
} polyhedronT;

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
  unsigned** edges;
  unsigned nedges;
} MeshT;
