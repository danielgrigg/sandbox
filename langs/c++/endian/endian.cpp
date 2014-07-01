// Very simple endian functions for known host endianness
#include <stdint.h>

template <int n>
void endian_swap(char *dst, char const *src);

template<> void endian_swap<2>(char* dest, char const* src)
{
    // Use bit manipulations instead of accessing individual bytes from memory, much faster.
    uint16_t* p_dest = reinterpret_cast< uint16_t* >(dest);
    uint16_t const* const p_src = reinterpret_cast< uint16_t const* >(src);
    *p_dest = (*p_src >> 8) | (*p_src << 8);
}
// Specialization for 4-byte types.
template<>
inline void endian_swap<4>(char* dest, char const* src)
{
    // Use bit manipulations instead of accessing individual bytes from memory, much faster.
    uint32_t* p_dest = reinterpret_cast< uint32_t* >(dest);
    uint32_t const* const p_src = reinterpret_cast< uint32_t const* >(src);
    *p_dest = (*p_src >> 24) | ((*p_src & 0x00ff0000) >> 8) | ((*p_src & 0x0000ff00) << 8) | (*p_src << 24);
}
//#define SwapFloat(x) endian_swap<4>(x, x)
//#define SwapShort(x) endian_swap<2>(x, x)

inline void SwapFloat(float *x)
{
  endian_swap<4>((char*)x, (char*)x);
}

#include <iostream>
#include <iomanip>

using namespace std;

int main()
{
  float x = 3.14519;
  uint32_t *y = (uint32_t*)&x; 
  cout << "host x = 0x" << hex << *y <<  endl;
  SwapFloat(&x); 
  cout << "target x = 0x" << hex << *y <<  endl;
  return 0;
}
