#ifndef LARRAY_H
#define LARRAY_H

#include <vector>

template <typename T>
class LArray2
{
public:
  LArray2(int width, int height):
    m_cells(width * height),
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
  
private:
   
  int m_width;
  int m_height;
  std::vector<T> m_cells; 
};

#endif
