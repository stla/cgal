#ifndef __VECTOR__
#define __VECTOR__
#include <vector>
#endif
#ifndef __STDDEF__
#define __STDDEF__
#include <stddef.h>
#endif

extern "C"
{

std::vector<int> iarray2vector(int*, size_t);
std::vector<double> darray2vector(double*, size_t);
double* dvector2array(std::vector<double>);
unsigned* uvector2array(std::vector<unsigned>);

}
