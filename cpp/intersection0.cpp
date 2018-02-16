#include <fstream>
#include <vector>
#include <CGAL/Simple_cartesian.h>
#include <CGAL/Polyhedron_incremental_builder_3.h>
#include <CGAL/Polyhedron_3.h>
#include <CGAL/IO/Polyhedron_iostream.h>

extern "C"
{

typedef CGAL::Simple_cartesian<double>     Kernel;
typedef CGAL::Polyhedron_3<Kernel>         Polyhedron;
typedef Polyhedron::HalfedgeDS             HalfedgeDS;

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
  std::vector<double> vs = darray2vector(vertices, nvertices);
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
  Polyhedron P = buildPolyhedron(vertices, 21, faces, facesizes, 2);
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



}
