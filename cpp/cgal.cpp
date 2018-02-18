#ifndef __VECTOR__
#define __VECTOR__
#include <vector>
#endif
#include <cstdlib> // to use malloc
//#include <algorithm> // to use std::find
#include "cgal.hpp"
#include "utils.hpp"
#include <fstream>

#include <CGAL/Exact_predicates_exact_constructions_kernel.h>
#include <CGAL/Polyhedron_incremental_builder_3.h>
#include <CGAL/Polyhedron_3.h>
#include <CGAL/IO/Polyhedron_iostream.h>
#include <CGAL/Nef_polyhedron_3.h>
#include <CGAL/Surface_mesh.h>
#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>
#include <CGAL/Gmpq.h>

#include <CGAL/Nef_3/SNC_indexed_items.h>
#include <CGAL/convex_decomposition_3.h>
#include <list>


extern "C"
{

typedef CGAL::Exact_predicates_exact_constructions_kernel Kernel;
typedef CGAL::Polyhedron_3<Kernel>                        Polyhedron;
typedef Polyhedron::HalfedgeDS                            HalfedgeDS;
typedef CGAL::Surface_mesh<Kernel::Point_3>               Surface_mesh;
//typedef CGAL::Nef_polyhedron_3<Kernel>                    Nef_polyhedron;
typedef Surface_mesh::Vertex_index                        vertex_descriptor;
typedef Surface_mesh::Face_index                          face_descriptor;
typedef Surface_mesh::Edge_index                          edge_descriptor;

typedef CGAL::Nef_polyhedron_3<Kernel, CGAL::SNC_indexed_items> Nef_polyhedron;
typedef Nef_polyhedron::Volume_const_iterator Volume_const_iterator;


double gmpq2double(CGAL::Gmpq r){
  //return r.numerator().to_double()/r.denominator().to_double();
  return r.to_double();
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
    B.begin_surface( coords.size()/3, facesizes.size()); // faces.size /3 ?
      /* add the polyhedron vertices */
      for(int i=0; i<(int)coords.size(); i+=3){
        B.add_vertex( Point( coords[i+0], coords[i+1], coords[i+2] ) );
      }
      // for(int i=0; i<(int)coords.size()/3; i++){
      //   if(std::find(faces.begin(), faces.end(), i) != faces.end()) { // checks if i belongs to faces - https://stackoverflow.com/questions/3450860/check-if-a-stdvector-contains-a-certain-object
      //     B.add_vertex( Point( coords[3*i+0], coords[3*i+1], coords[3*i+2] ) );
      //   }
      // }
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


MeshT surfacemeshToMesh(Surface_mesh smesh){
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
      // std::cout << smesh.point(vd) << std::endl;
      vertices[i_vertex].point = (double*)malloc(3 * sizeof(double));
      for(unsigned k=0; k < 3; k++){
        vertices[i_vertex].point[k] = gmpq2double(smesh.point(vd)[k].exact());
      }
      i_vertex++;
    }
  }
  /* faces */
  printf("Number of faces: %d\n", smesh.number_of_faces());
  FaceT* faces = new FaceT[smesh.number_of_faces()];
  unsigned* facesSizes = (unsigned*)malloc(smesh.number_of_faces() * sizeof(unsigned));
  std::cout << "Iterate over faces\n";
  {
    unsigned i_face = 0;
    BOOST_FOREACH(face_descriptor fd, smesh.faces()){
      // std::cout << smesh.halfedge(fd) << std::endl;
      std::vector<unsigned> verticesIds;
      facesSizes[i_face] = 0;
      BOOST_FOREACH(vertex_descriptor vd, vertices_around_face(smesh.halfedge(fd), smesh)){
        //std::cout << vd << std::endl;
        // printf("vertex: %u\n", vd);
        verticesIds.push_back(vd);
        facesSizes[i_face]++;
      }
      faces[i_face].verticesIds = uvector2array(verticesIds);
      faces[i_face].nvertices = facesSizes[i_face];
      i_face++;
    }
  }
  /* output mesh */
  MeshT out;
  out.vertices = vertices;
  out.nvertices = smesh.number_of_vertices();
  out.faces = faces;
  out.faceSizes = facesSizes;
  out.nfaces = smesh.number_of_faces();
  out.edges = edges;
  out.nedges = smesh.number_of_edges();
  return out;
}


MeshT* intersectPolyhedra(Polyhedron P1, Polyhedron P2){
  MeshT* out = (MeshT*)malloc(sizeof(MeshT));
  /* convert polyhedra to nefs */
  printf("make nef1\n");
  Nef_polyhedron nef1(P1);
  printf("make nef2\n");
  Nef_polyhedron nef2(P2);
  /* compute the intersection */
  printf("make nefs intersection\n");
  Nef_polyhedron nef = nef1*nef2;
  /* surface mesh */
  Surface_mesh smesh;
  CGAL::convert_nef_polyhedron_to_polygon_mesh(nef, smesh);
  /* write OFF file */
  std::ofstream outfile;
  outfile.open("intersection.off");
  outfile << smesh;
  outfile.close();
  /* output */
  *out = surfacemeshToMesh(smesh);
  return out;
}

MeshT* unitePolyhedra(Polyhedron P1, Polyhedron P2){
  MeshT* out = (MeshT*)malloc(sizeof(MeshT));
  /* convert polyhedra to nefs */
  printf("make nef1\n");
  Nef_polyhedron nef1(P1);
  printf("make nef2\n");
  Nef_polyhedron nef2(P2);
  /* compute the intersection */
  printf("make nefs union\n");
  Nef_polyhedron nef = nef1 + nef2;
  /* surface mesh */
  Surface_mesh smesh;
  CGAL::convert_nef_polyhedron_to_polygon_mesh(nef, smesh);
  /* write OFF file */
  std::ofstream outfile;
  outfile.open("union.off");
  outfile << smesh;
  outfile.close();
  /* output */
  *out = surfacemeshToMesh(smesh);
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
  printf("build P1\n");
  Polyhedron P1 = buildPolyhedron(vertices1, nvertices1, faces1, facesizes1, nfaces1);
  printf("P1 is closed: %u\n", P1.is_closed());
  printf("P1 is valid: %u\n", P1.is_valid());
  // std::cout << P1;
  printf("build P2\n");
  Polyhedron P2 = buildPolyhedron(vertices2, nvertices2, faces2, facesizes2, nfaces2);
  printf("P2 is closed: %u\n", P2.is_closed());
  printf("P2 is valid: %u\n", P2.is_valid());
  // std::cout << P2;
  printf("run intersection\n");
  MeshT* mesh = intersectPolyhedra(P1, P2);
  return mesh;
}

MeshT* unionTwoPolyhedra(
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
  printf("P1 is closed: %u\n", P1.is_closed());
  printf("P1 is valid: %u\n", P1.is_valid());
  Polyhedron P2 = buildPolyhedron(vertices2, nvertices2, faces2, facesizes2, nfaces2);
  printf("P2 is closed: %u\n", P2.is_closed());
  printf("P2 is valid: %u\n", P2.is_valid());
  printf("run union\n");
  MeshT* mesh = unitePolyhedra(P1, P2);
  return mesh;
}

Surface_mesh polyhedron2surfacemesh(Polyhedron P){
    Nef_polyhedron nef(P);
    Surface_mesh smesh;
    CGAL::convert_nef_polyhedron_to_polygon_mesh(nef, smesh);
    return smesh;
}

MeshT* convexParts(
  double* vertices,
  size_t nvertices,
  int* faces,
  int* facesizes,
  size_t nfaces,
  size_t* nparts)
{
  Polyhedron P0 = buildPolyhedron(vertices, nvertices, faces, facesizes, nfaces);
  // TODO: check P0 is closed...
  Nef_polyhedron N(P0);
  if(N.is_simple()) {
    N.convert_to_polyhedron(P0);
  }else{
    std::cerr << "N is not a 2-manifold." << std::endl;
  }
  CGAL::convex_decomposition_3(N);
  std::list<Polyhedron> convex_parts;
  // the first volume is the outer volume, which is ignored in the decomposition
  Volume_const_iterator ci = ++N.volumes_begin();
  for( ; ci != N.volumes_end(); ++ci){
    if(ci->mark()) {
      Polyhedron P;
      N.convert_inner_shell_to_polyhedron(ci->shells_begin(), P);
      convex_parts.push_back(P);
      // Surface_mesh sm = polyhedron2surfacemesh(P);
      // std::cout << sm;
    }
  }
  *nparts = convex_parts.size();
  std::cout << "decomposition into " << *nparts << " convex parts " << std::endl;
  /* output */
  MeshT* out = (MeshT*)malloc(*nparts * sizeof(MeshT));
  size_t i = 0;
  for(Polyhedron polyh : convex_parts){
    Surface_mesh smesh = polyhedron2surfacemesh(polyh);
    out[i] = surfacemeshToMesh(smesh);
    /* write OFF file */
    char filename [50];
    sprintf(filename, "cgal%04Iu.off", i);
    std::ofstream outfile;
    outfile.open(filename);
    outfile << smesh;
    outfile.close();
    i++;
  }
  return out;
}


void polyhedron2off(
  double* vertices,
  size_t nvertices,
  int* faces,
  int* facesizes,
  size_t nfaces,
  char* outfile)
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
  /* write the polyhedron out as a .OFF file */
  std::ofstream os(outfile);
  os << P;
  os.close();
}



}
