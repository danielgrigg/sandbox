#include <iostream>
#include <string>
#include <vector>
#include <fstream>

#include "../shared/ObjImport.hpp"
#include "../shared/ObjManip.hpp"
#include "../shared/MeshCompute.h"

#include <boost/filesystem/operations.hpp> 
#include <boost/filesystem/fstream.hpp>   
#include <boost/filesystem/convenience.hpp>

using namespace boost::filesystem;  
using namespace lmc;
using namespace std;

void printBatches(const string& jsFile, 
    vector<IndexedTriangleBatch>& batches)
{
  std::ofstream fout(jsFile.c_str());
  fout << "var " << basename(jsFile) << " =\n";
  fout << "[\n";
  for (int i = 0; i < batches.size(); ++i)
  {
    fout << batches[i].toJSON();
    if (i < batches.size() - 1)
    {
      fout << ",\n";
    }
  }
  fout << "];\n";
}

int main(int argc, char **argv)
{
  if (argc < 3)
  {
    cerr << "Usage: objson <objfile> <jsfile> [batch-size]\n";
    return 1;
  }
  const string modelFile = argv[1];
  const string jsFile = argv[2];

  ObjImport model;
  if (!model.import(modelFile))
  {
    cerr << "Error importing '" << modelFile << "'\n";
    return 1;
  }

  IndexedTriangleBatch modelBatch;
  cout << "Importing " << modelFile << "..." << flush;
  if (!makeIndexedTriangleBatchFromObjImport(model, modelBatch))
  {
    cerr << "Error batching '" << modelFile << "'\n";
    return 1;
  }
  cout << "done" << endl;

  // Default to 16-bit range batches
  uint32_t chunkSize = argc < 4 ? 65535 / 3 : strtol(argv[3], NULL, 10);

  if (chunkSize == 0)
  {
    cout << modelBatch.toJSON() << endl;
    return 0;
  }

  vector<IndexedTriangleBatch> chunks;

  cout << "Partitioning..." << flush;
  splitBatch(modelBatch, chunkSize, chunks);

  cout << chunks.size() << " chunk(s) created:\n";
  for (int i = 0; i < chunks.size(); ++i)
  {
    cerr << "  [" << i << "]: " << chunks[i].is.size()/3 << " triangles, "
      << chunks[i].vs.size() << " vertices.\n";
  }
  cout << "Exporting to '" << jsFile << "'... " << flush;
  printBatches(jsFile, chunks);
  cout << "done" << endl;
  return 0;
}

