#ifndef __VECTOR__
#define __VECTOR__
#include <vector>
#endif
#include <cstdlib> // to use malloc
#include "intersection.hpp"
#include "utils.hpp"
#include <fstream>
//#include <CGAL/Simple_cartesian.h>
#include <CGAL/Exact_predicates_exact_constructions_kernel.h>
#include <CGAL/Polyhedron_incremental_builder_3.h>
#include <CGAL/Polyhedron_3.h>
#include <CGAL/IO/Polyhedron_iostream.h>

#include <CGAL/Nef_polyhedron_3.h>
#include <CGAL/Surface_mesh.h>
#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>

#include <CGAL/Gmpq.h>
// #include <CGAL/Gmpz.h>

extern "C"
{

//typedef CGAL::Simple_cartesian<double>     Kernel;
typedef CGAL::Exact_predicates_exact_constructions_kernel Kernel;
typedef CGAL::Polyhedron_3<Kernel>                        Polyhedron;
typedef Polyhedron::HalfedgeDS                            HalfedgeDS;

typedef CGAL::Surface_mesh<Kernel::Point_3>               Surface_mesh;
typedef CGAL::Nef_polyhedron_3<Kernel>                    Nef_polyhedron;

typedef Surface_mesh::Vertex_index                        vertex_descriptor;
typedef Surface_mesh::Face_index                          face_descriptor;
typedef Surface_mesh::Edge_index                          edge_descriptor;

double gmpq2double(CGAL::Gmpq r){
  return r.numerator().to_double()/r.denominator().to_double();
}

// A modifier creating a polyhedron with the incremental builder.
class polyhedron_builder : public CGAL::Modifier_base<HalfedgeDS> {
public:
  std::vector<double> &coords;
  std::vector<int>    &faces;
  std::vector<int>    &facesizes;
  polyhedron_builder(
    std::vector<double> &_coords,
    std::vector<int> &_faces,
    std::vector<int> &_facesizes
  ) : coords(_coords), faces(_faces), facesizes(_facesizes) {}
  void operator()( HalfedgeDS& hds) {
    typedef typename HalfedgeDS::Vertex   Vertex;
    typedef typename Vertex::Point        Point;
    /* create a cgal incremental builder */
    CGAL::Polyhedron_incremental_builder_3<HalfedgeDS> B( hds, true);
    B.begin_surface( coords.size()/3, faces.size()/3 );
      /* add the polyhedron vertices */
      for(int i=0; i<(int)coords.size(); i+=3){
        B.add_vertex( Point( coords[i+0], coords[i+1], coords[i+2] ) );
      }
      /* add the polyhedron faces */
      int i=0;
      for(int k=0; k<(int)facesizes.size(); k++){
        int fs = facesizes[k];
        B.begin_facet();
        for(int j=0; j<fs; j++){
          B.add_vertex_to_facet( faces[i+j] );
        }
        B.end_facet();
        i += fs;
      }
    /* finish up the surface */
    B.end_surface();
  }
};


Polyhedron buildPolyhedron(
  double* vertices,
  size_t nvertices,
  int* faces,
  int* facesizes,
  size_t nfaces)
{
  /* calculate length of `faces`*/
  size_t l = 0;
  for(size_t i=0; i < nfaces; i++){
    l += facesizes[i];
  }
  /* make vectors */
  std::vector<double> vs = darray2vector(vertices, 3*nvertices);
  std::vector<int> fs = iarray2vector(faces, l);
  std::vector<int> fzs = iarray2vector(facesizes, nfaces);
  /* build the polyhedron */
  Polyhedron P;
  polyhedron_builder builder(vs, fs, fzs);
  P.delegate( builder );
  /**/
  return P;
}


MeshT* intersectPolyhedra(Polyhedron P1, Polyhedron P2){
  MeshT* out = (MeshT*)malloc(sizeof(MeshT));
  /* convert polyhedra to nefs */
  Nef_polyhedron nef1(P1);
  Nef_polyhedron nef2(P2);
  /* compute the intersection */
  Nef_polyhedron nef = nef1*nef2;
  /* surface mesh */
  Surface_mesh smesh;
  CGAL::convert_nef_polyhedron_to_polygon_mesh(nef, smesh);
  /* edges */
  printf("Number of edges: %d\n", smesh.number_of_edges());
  unsigned** edges = (unsigned**)malloc(smesh.number_of_edges() * sizeof(unsigned*));
  std::cout << "Iterate over edges\n";
  {
    unsigned i_edge = 0;
    BOOST_FOREACH(edge_descriptor ed, smesh.edges()){
      edges[i_edge] = (unsigned*)malloc(2 * sizeof(unsigned));
      edges[i_edge][0] = source(ed,smesh);
      edges[i_edge][1] = target(ed,smesh);
      i_edge++;
    }
  }
  /* vertices */
  printf("Number of vertices: %d\n", smesh.number_of_vertices());
  VertexT* vertices = (VertexT*)malloc(smesh.number_of_vertices() * sizeof(VertexT));
  std::cout << "Iterate over vertices\n";
  {
    unsigned i_vertex = 0;
    BOOST_FOREACH(vertex_descriptor vd, smesh.vertices()){
      std::cout << smesh.point(vd) << std::endl;
      vertices[i_vertex].point = (double*)malloc(3 * sizeof(double));
      for(unsigned k=0; k < 3; k++){
        vertices[i_vertex].point[k] = gmpq2double(smesh.point(vd)[k].exact());
      }
      i_vertex++;
    } // smesh.point(vd) is a vector: smesh.point(vd)[0] gives first component; no...
  }
  /* faces */
  printf("Number of faces: %d\n", smesh.number_of_faces());
  FaceT* faces = new FaceT[smesh.number_of_faces()];
  unsigned* facesSizes = (unsigned*)malloc(smesh.number_of_faces() * sizeof(unsigned));
  std::cout << "Iterate over faces\n";
  {
    unsigned i_face = 0;
    BOOST_FOREACH(face_descriptor fd, smesh.faces()){
      std::cout << smesh.halfedge(fd) << std::endl;
      std::vector<unsigned> verticesIds;
      facesSizes[i_face] = 0;
      BOOST_FOREACH(vertex_descriptor vd, vertices_around_face(smesh.halfedge(fd), smesh)){
        //std::cout << vd << std::endl;
        printf("vertex: %u\n", vd);
        verticesIds.push_back(vd);
        facesSizes[i_face]++;
      }
      faces[i_face].verticesIds = uvector2array(verticesIds);
      faces[i_face].nvertices = facesSizes[i_face];
      i_face++;
    }
  }
  /* write OFF file */
  std::ofstream outfile;
  outfile.open("intersection.off");
  outfile << smesh;
  outfile.close();
  /* output mesh */
  out->vertices = vertices;
  out->nvertices = smesh.number_of_vertices();
  out->faces = faces;
  out->faceSizes = facesSizes;
  out->nfaces = smesh.number_of_faces();
  out->edges = edges;
  out->nedges = smesh.number_of_edges();
  return out;
}


MeshT* intersectionTwoPolyhedra(
  double* vertices1,
  size_t nvertices1,
  int* faces1,
  int* facesizes1,
  size_t nfaces1,
  double* vertices2,
  size_t nvertices2,
  int* faces2,
  int* facesizes2,
  size_t nfaces2)
{
  Polyhedron P1 = buildPolyhedron(vertices1, nvertices1, faces1, facesizes1, nfaces1);
  Polyhedron P2 = buildPolyhedron(vertices2, nvertices2, faces2, facesizes2, nfaces2);
  MeshT* mesh = intersectPolyhedra(P1, P2);
  return mesh;
}

}
