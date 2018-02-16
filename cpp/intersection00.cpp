#include <fstream>
#include <vector>
//#include <CGAL/Simple_cartesian.h>
#include <CGAL/Exact_predicates_exact_constructions_kernel.h>
#include <CGAL/Polyhedron_incremental_builder_3.h>
#include <CGAL/Polyhedron_3.h>
#include <CGAL/IO/Polyhedron_iostream.h>

#include <CGAL/Nef_polyhedron_3.h>
#include <CGAL/Surface_mesh.h>
#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>

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

std::vector<int> iarray2vector(int* array, size_t n){
  std::vector<int> out;
  for(size_t i=0; i<n; i++){
      out.push_back(array[i]);
  }
  return out;
}
std::vector<double> darray2vector(double* array, size_t n){
  std::vector<double> out;
  for(size_t i=0; i<n; i++){
      out.push_back(array[i]);
  }
  return out;
}

// A modifier creating a triangle with the incremental builder.
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
    /* add the polyhedron triangles */
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


Polyhedron makePolyhedron(){
  /* vertices */
  double _coords[21]  = {1,0,0, 0,1,0, 0,0,1, 2,0,0, 0,2,0, 0,0,2, 1,0,1};
  /* faces given by vertex indices */
  int    _faces[7]    = {0,1,2, 3,4,5,6};
  /* face sizes */
  int    _facesizes[2] = {3,4};
  /* convert to vectors */
  std::vector<double> coords = darray2vector(_coords, 21);
  std::vector<int> faces = iarray2vector(_faces, 7);
  std::vector<int> facesizes = iarray2vector(_facesizes, 2);
  /* build the polyhedron */
  Polyhedron P;
  polyhedron_builder builder( coords, faces, facesizes);
  P.delegate( builder );
  /**/
  return P;
}

int test() {
  Polyhedron P = makePolyhedron();
  /* write the polyhedron out as a .OFF file */
  std::ofstream os("intersection.off");
  os << P;
  os.close();

  return 0;
}

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

int test2() {
  /* vertices */
  double vertices[21] = {1,0,0, 0,1,0, 0,0,1, 2,0,0, 0,2,0, 0,0,2, 1,0,1};
  /* faces given by vertex indices */
  int faces[7] = {0,1,2, 3,4,5,6};
  /* face sizes */
  int facesizes[2] = {3,4};
  /* build polyhedron */
  Polyhedron P = buildPolyhedron(vertices, 7, faces, facesizes, 2);
  /* write the polyhedron out as a .OFF file */
  std::ofstream os("intersection.off");
  os << P;
  os.close();

  return 0;
}

void polyhedron2off(
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
  /* write the polyhedron out as a .OFF file */
  std::ofstream os("intersection.off");
  os << P;
  os.close();
}

void intersectPolyhedra(Polyhedron P1, Polyhedron P2){
  Nef_polyhedron nef1(P1);
  Nef_polyhedron nef2(P2);
  /* compute the intersection */
  Nef_polyhedron nef = nef1*nef2;
  /* output */
  Surface_mesh output;
  CGAL::convert_nef_polyhedron_to_polygon_mesh(nef, output);

  printf("Number of vertices: %d\n", output.number_of_vertices());
  std::cout << "Iterate over vertices\n";
  {
    BOOST_FOREACH(vertex_descriptor vd, output.vertices()){
      std::cout << output.point(vd) << std::endl;
    } // output.point(vd) is a vector: output.point(vd)[0] gives first component
  }

  printf("Number of faces: %d\n", output.number_of_faces());
  std::cout << "Iterate over faces\n";
  {
    BOOST_FOREACH(face_descriptor fd, output.faces()){
      std::cout << output.halfedge(fd) << std::endl;
      BOOST_FOREACH(vertex_descriptor vd, vertices_around_face(output.halfedge(fd), output)){
        //std::cout << vd << std::endl;
        printf("vertex: %u\n", vd);
      }
    }
  }

  std::ofstream out;
  out.open("intersection.off");
  out << output;
  out.close();
}

void testintersection() {
  /* vertices */
  double vertices1[24] = {-1,-1,-1,
                          -1,-1, 1,
                          -1, 1,-1,
                          -1, 1, 1,
                           1,-1,-1,
                           1,-1, 1,
                           1, 1,-1,
                           1, 1, 1 };
  int faces[24] = {0,2,6,4, 5,7,3,1, 4,5,1,0, 2,3,7,6, 0,1,3,2, 6,7,5,4};
  int facesizes[6] = {4,4,4,4,4,4};
  double vertices2[24] = {-0.5,-0.5,-0.5,
                          -0.5,-0.5, 1.5,
                          -0.5, 1.5,-0.5,
                          -0.5, 1.5, 1.5,
                           1.5,-0.5,-0.5,
                           1.5,-0.5, 1.5,
                           1.5, 1.5,-0.5,
                           1.5, 1.5, 1.5 };
  Polyhedron P1 = buildPolyhedron(vertices1, 8, faces, facesizes, 6);
  Polyhedron P2 = buildPolyhedron(vertices2, 8, faces, facesizes, 6);
  intersectPolyhedra(P1, P2);
}

int intersectionTwoPolyhedra(
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
  // printf("START\n");
  // /* calculate length of `faces`*/
  // size_t l1 = 0;
  // for(size_t i=0; i < nfaces1; i++){
  //   l1 += facesizes1[i];
  // }
  // size_t l2 = 0;
  // for(size_t i=0; i < nfaces2; i++){
  //   l2 += facesizes2[i];
  // }
  // /* make vectors */
  // printf("MAKE VECTORS\n");
  // std::vector<double> vs1 = darray2vector(vertices1, 3*nvertices1);
  // std::vector<int> fs1 = iarray2vector(faces1, l1);
  // std::vector<int> fzs1 = iarray2vector(facesizes1, nfaces1);
  // std::vector<double> vs2 = darray2vector(vertices2, 3*nvertices2);
  // std::vector<int> fs2 = iarray2vector(faces2, l2);
  // std::vector<int> fzs2 = iarray2vector(facesizes2, nfaces2);
  // /* build the polyhedra */
  // printf("BUILD P1\n");
  // Polyhedron P1;
  // polyhedron_builder builder1(vs1, fs1, fzs1);
  // P1.delegate(builder1);
  // printf("BUILD P2\n");
  // Polyhedron P2;
  // polyhedron_builder builder2(vs2, fs2, fzs2);
  // P2.delegate(builder2);
  /* intersection */
  intersectPolyhedra(P1, P2);
  return 0;
}

}
