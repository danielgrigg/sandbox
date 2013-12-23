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

#include "../shared/ObjImport.hpp"
#include "../shared/ObjManip.hpp"
#include "../shared/MeshCompute.h"

using namespace lmc;

using namespace std;

int main(int argc, char **argv)
{
  if (argc < 3)
  {
    cerr << "Usage: lbatchify <objfile> <batch-size>\n";
    return 1;
  }
  const string modelFile = argv[1];
  uint32_t chunkSize = strtol(argv[2], NULL, 10);

  ObjImport model;
  if (!model.import(modelFile))
  {
    cerr << "Error importing '" << modelFile << "'\n";
    return 1;
  }

  IndexedTriangleBatch modelBatch;
  if (!makeIndexedTriangleBatchFromObjImport(model, modelBatch))
  {
    cerr << "Error batching '" << modelFile << "'\n";
    return 1;
  }

  cout << "ModelBatch #tris " << modelBatch.is.size()/3 << 
    ", #is " << modelBatch.is.size();
  cout << ", #vs " << modelBatch.vs.size() << endl;
  if (chunkSize == 0)
  {
    cout << modelBatch.toJSON() << endl;
    return 0;
  }

  vector<IndexedTriangleBatch> chunks;
  splitBatch(modelBatch, chunkSize, chunks);

  if (0)
  {
    for (int i = 0; i < chunks.size(); ++i)
    {
      cout << "Chunk #tris " << chunks[i].is.size()/3 << 
        ", #is " << chunks[i].is.size();
      cout << ", #vs " << chunks[i].vs.size() << endl;
    }
  }

  if (0)
  {
    for (int i = 0; i < chunks.size(); ++i)
    {
      cout << "Chunk" << i << endl;
      cout << chunks[i] << endl;;
    }
  }

  for (int i = 0; i < chunks.size(); ++i)
  {
    cout << chunks[i].toJSON() << endl;
  }
  return 0;
}
