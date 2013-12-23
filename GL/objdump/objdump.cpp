#include <iostream>
#include <string>
#include <vector>
#include <fstream>

#include "../shared/ObjModel.h"
//#include "../shared/MeshCompute.h"

using namespace lmc;
using namespace std;

int main(int argc, char **argv)
{
  if (argc < 3)
  {
    cerr << "Usage: objdump <objfile> [outobjfile]\n";
    return 1;
  }
  const string modelFile = argv[1];
  const string outModelFile = argv[2];

  ObjModel model;
  ObjTranslator ot;
  if (!ot.importFile(modelFile, &model))
  {
    cerr << "Error importing '" << modelFile << "'\n";
    return 1;
  }

//  cout << model << endl;

  cout << "HasVertices: " << !model.vertices.empty() << endl;
  cout << "HasUVS: " << !model.uvs.empty() << endl;
  cout << "HasNormals: " << !model.normals.empty() << endl;
  cout << model.vertices.size() << " vertices\n";
  cout << model.faceIndices.size() / model.numComponents() / 3 << " triangles.\n";

  ot.exportFile(&model, outModelFile);
  return 0;
}
