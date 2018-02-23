#ifndef __VECTOR__
#define __VECTOR__
#include <vector>
#endif
#ifndef __STDDEF__
#define __STDDEF__
#include <stddef.h>
#endif

#include "utils.hpp"

extern "C"
{

std::vector<unsigned> uarray2vector(unsigned* array, size_t n){
  std::vector<unsigned> out;
  for(size_t i=0; i<n; i++){
      out.push_back(array[i]);
  }
  return out;
}
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

double* dvector2array(std::vector<double> vec){
  size_t n = vec.size();
  double* out = new double[n];
  for(size_t i=0; i < n; i++){
    out[i] = vec[i];
  }
  return out;
}

unsigned* uvector2array(std::vector<unsigned> vec){
  size_t n = vec.size();
  unsigned* out = new unsigned[n];
  for(size_t i=0; i < n; i++){
    out[i] = vec[i];
  }
  return out;
}

}
