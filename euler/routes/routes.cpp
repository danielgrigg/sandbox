#include <stdint.h>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <iterator>
#include <iomanip>
#include <string>
#include <vector>
#include <map>

using namespace std;

template <typename T>
class LArray2
{
public:
  LArray2(int width, int height, T seed):
    m_cells(width * height, seed),
    m_width(width),
    m_height(height)
  {
        
  }
  
  const T & operator()(int x, int y)const
  {
    return m_cells[y * m_width + x];
  }
  T & operator()(int x, int y)
  {
    return m_cells[y * m_width + x];
  }

  operator const T *()
  {
    return &m_cells[0];
  }
  
  int width()const { return m_width; }
  int height()const { return m_height; }

  T *beginRow(int row){ return &((*this)(0, row)); }
  const T *beginRow(int row)const { return &((*this)(0, row)); }
  T *begin() { return &m_cells[0]; }
  const T *begin()const { return &m_cells[0]; }
  T *endRow(int row){ return &((*this)(0, row+1)); }
  const T *endRow(int row)const{ return &((*this)(0, row+1)); }
  T *end() { return &m_cells[width() * height()]; }
  const T *end()const { return &m_cells[width() * height()]; }
  
private:
  int m_width;
  int m_height;
  vector<T> m_cells; 
};

template <typename T>
std::ostream & operator <<(std::ostream &os, const LArray2<T> &rhs)
{
  ostream_iterator<int64_t> osit(os, " | ");
  for (int y = 0; y < rhs.height(); ++y)
  {
    os << "\n| ";
    copy(rhs.beginRow(y), rhs.endRow(y), osit);
  }
  os << "\n";
}

struct _unique
{
  int current;
  _unique(){current = 0; }
  int operator()() { return current++; }
} Unique;

int main(int argc, char **argv)
{
  int gridSize = 8;
  if (argc > 1)
  {
    gridSize = strtol(argv[1], NULL, 10); 
  }

  cout << "Calculating route combinations for " 
    << gridSize << "x" << gridSize << " grid." << endl;

  // Pad the grid with an extra column and row of ones to seed summation.
  // Note the resulting grid is symmetric thus we could limit computation to the diagonal but meh.
  int paddedSize = gridSize + 1;
  LArray2<int64_t> routes(paddedSize,paddedSize,1);
  for (int y = 1; y < routes.height(); ++y)
  {
    for (int x = 1; x < routes.width(); ++x)
    {
      routes(x, y) = routes(x-1, y) + routes(x, y-1);
    }
  }
  cerr << routes << endl;

  cout << "Route combinations for " << gridSize << "x" << gridSize 
        << " grid = " << routes(routes.width()-1, routes.height()-1) << endl;

  // Note the exponential growth rate of ~3, suggesting there's a undoubtedly some simple 
  // formula to solve this.  Too late now!
  return 0;
}
