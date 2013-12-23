#include <algorithm>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <map>
#include <numeric>
#include <sstream>
#include <stdint.h>
#include <string>
#include <vector>
   
//bool operator ==(const allocator<T>&, const allocator<T>&);
//bool operator !=(const allocator<T>&, const allocator<T>&);

// Example of creating a custom allocator based on effective stl item 11.

template<typename T>
class SharedMemoryAllocator
{
  public:
    typedef size_t     size_type;
    typedef ptrdiff_t  difference_type;
    typedef T*         pointer;
    typedef const T*   const_pointer;
    typedef T&         reference;
    typedef const T&   const_reference;
    typedef T          value_type;
       
//    template <class U> SharedMemoryAllocator(const allocator<U>&);
    template <class U> struct rebind 
    { 
      typedef SharedMemoryAllocator<U> other; 
    };

    T*        address(T& o) const  { return &o; }
    const T*  address(const T& o) const { return &o; }
    void      construct(T* p, const T& val) { new ((void*)p) T(val); }
    void      destroy(T*p) { ((T*)p)->~T(); }
    size_type max_size() const { return ~0; }


    pointer allocate(uint64_t numObjects, const void* localityHint = 0)
    {
      std::cout << "allocate(" << numObjects << ")\n";
      return static_cast<pointer>(malloc(numObjects * sizeof(T)));
    }
    void deallocate(pointer ptr, uint64_t numObjects)
    {
      std::cout << "deallocate(" << numObjects << ")\n";
      free(ptr);
    }
};


