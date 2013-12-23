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
#include "ObjImport.hpp"
#include "LStopwatch.hpp"
#include "ObjManip.hpp"

using namespace std;

void indexPath1(const char* name)
{
  ObjImport obj;
  LStopwatch timer;
  timer.reset();
  if (!obj.import(name))
  {
    std::cerr << "error importing" << endl;
    return;
  }
  double dt = timer.timeElapsed_s();
    std::cout << "Imported in " << dt << " s.\n";
  //  std::cout << "ObjImport:\n" << obj << endl;
  
  TriangleBatch batch;
  timer.reset();
  makeTriangleBatchFromObjImport(obj, batch);
  dt = timer.timeElapsed_s();
  std::cout << "Made regular batch in " << dt << "s.\n";

  IndexedTriangleBatch indexedBatch;
  timer.reset();
  makeIndexedTriangleBatchFromTriangleBatch(batch, indexedBatch);
  double dt2 = timer.timeElapsed_s();
  std::cout << "Indexed batch in " << dt2 << "s.\n";
  std::cout << "Total batching time:  " << dt+dt2 << "s.\n";

  std::cout << "\nIndexedBatch:\n" << indexedBatch << endl;
}

void indexPath2(const char* name)
{
  ObjImport obj;
  LStopwatch timer;
  timer.reset();
  if (!obj.import(name))
  {
    std::cerr << "error importing" << endl;
    return;
  }
  double dt = timer.timeElapsed_s();
    std::cout << "\nImported in " << dt << " s.\n";
  //  std::cout << "ObjImport:\n" << obj << endl;

  IndexedTriangleBatch batch;
  timer.reset();
  makeIndexedTriangleBatchFromObjImport(obj, batch);
  dt = timer.timeElapsed_s();

  std::cout << "Made fast indexed batch in " << dt << "s.\n";

  std::cout << "\nIndexedBatch:\n" << batch << endl;
} 

int main(int argc, char **argv)
{
  const char* toImport = argc < 2 ? "pyramid.obj" : argv[1];
  indexPath1(toImport);
  indexPath2(toImport);
  
#if 0
  {
    for (uint32_t i = 0; i < batch.vs.size(); ++i)
      cout << batch.vs[i] << ", " << batch.uvs[i] << ", " << batch.ns[i] <<  endl;

    std::cout << "\nIndexedBatch expanded:\n"  << endl;
    for (uint32_t i = 0; i < indexedBatch.is.size(); ++i)
    {
      cerr << indexedBatch.vs[indexedBatch.is[i]] << ", " << 
        indexedBatch.uvs[indexedBatch.is[i]] << ", " << 
        indexedBatch.ns[indexedBatch.is[i]] <<  endl;
    }
  }
#endif

  return 0;
}
