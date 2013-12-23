#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include "../shared/ObjModel.h"
#include "../shared/MeshCompute.h"

using namespace lmc;
using namespace std;

int main(int argc, char **argv)
{
  if (argc < 2)
  {
    cerr << "Usage: meshdump <objfile>\n";
    return 1;
  }
  const string modelFile = argv[1];
  
  Mesh mesh;
  if (!mesh.importFromObjFile(modelFile))
  {
    cerr << "Error importing " << modelFile << endl;
    return 1;
  }
  if (argc < 3)
  {
    cout << mesh << endl;
  }
  else
  {
    Mesh materialSorted;
    mesh.mergeMaterialGroups(materialSorted);
    cout << "materialSorted\n" << materialSorted << endl;

    vector<Mesh> chunks;
    materialSorted.partition(chunks, strtol(argv[2], NULL, 10));

    std::ostream_iterator<Mesh> osi(cout, "\n");
    std::copy(chunks.begin(), chunks.end(), osi);
  }
  
  return 0;
}
