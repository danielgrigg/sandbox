#ifndef MESH_MATH_H
#define MESH_MATH_H

#include <iostream>
#include <algorithm>

namespace lmc 
{
  template<int N>
    class vec
    {
      public:
      vec()
      {
        std::fill(v, v+N, float());
      }

      bool operator==(const vec<N>& rhs)const
      {
        return std::equal(v, v+N, rhs.v);
      }

      vec<N>& operator=(const vec<N>& rhs)
      {
        std::copy(rhs.v, rhs.v + N, v);
        return *this;
      }

      float operator[](int i)const { return v[i]; }
      float& operator[](int i) { return v[i]; }

      private:
      float v[N];
    };

  template <int N>
    std::ostream& operator<<(std::ostream& os, const vec<N>& rhs)
    {
      for (int i = 0; i < N-1; ++i) os << rhs[i] << ' ';
      os << rhs[N-1];
    }

  typedef vec<3> vec3f;
  typedef vec<2> vec2f;

  template<int N>
  vec<N> parseVec(char* context)
  {
    vec<N> v;
    for (int i = 0; i < N; ++i) 
      v[i] = strtof(strtok_r(NULL, " ", &context), NULL);
    return v;
  }
}

#endif
