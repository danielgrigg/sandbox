#include "ObjModel.h"
#include <sstream>
#include <fstream>
#include <cassert>
#include <iostream>
#include <iterator>
#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/fstream.hpp>    // ditto
#include <boost/filesystem/convenience.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>

using namespace boost::lambda;

namespace lmc {

  inline bool CStringEqual(const char* a, const char* b)
  {
    return strcmp(a, b) == 0;
  }

  void MtlTranslator::parseLine(char* line)
  {
    char* context;
    char* token = strtok_r(line, " ", &context);
    if (token == NULL) return;

    if (token[0] == '#') return;

    if (CStringEqual(token, "newmtl"))
    {
      _working = strtok_r(NULL,"\n",&context);
      _model->materials[_working] = ObjMaterial(_working);
    }
    else if (CStringEqual(token, "illum"))
    {
      //materials.back().illum = strtof(strtok_r(NULL, "\n", &context), NULL);
      //      std::cerr << "illum unsupported - skipping" << std::endl;
    }
    else if (CStringEqual(token, "Kd"))
    {
      working().Kd = parseVec<3>(context);
    }
    else if (CStringEqual(token, "Ka"))
    {
      working().Ka = parseVec<3>(context);
    }
    else if (CStringEqual(token, "Tf"))
    {
      working().Tf = parseVec<3>(context);
    }
    else if (CStringEqual(token, "Ni"))
    {
      working().Ni = strtof(strtok_r(NULL, "\n", &context), NULL);
    }
    else if (CStringEqual(token, "Ns"))
    {
      working().Ns = strtof(strtok_r(NULL, "\n", &context), NULL);
    }
    else if (CStringEqual(token, "Ks"))
    {
      working().Ks = parseVec<3>(context);
    }
    else if (CStringEqual(token, "map_Ka"))
    {
      working().map_Ka = strtok_r(NULL, "\n", &context);
    }
    else if (CStringEqual(token, "map_Kd"))
    {
      working().map_Kd = strtok_r(NULL, "\n", &context);
    }
    else if (CStringEqual(token, "map_Ks"))
    {
      working().map_Ks = strtok_r(NULL, "\n", &context);
    }
    else
    {
      std::cerr << "ObjMaterial import error: unrecognised token '" 
        << token << "'\n";
    }
  }

  bool MtlTranslator::importFile(const std::string& filename, ObjModel* model)
  {
    _model = model;
    std::fstream fs (filename.c_str(), std::fstream::in);
    if (!fs.is_open()) return false;
    char line[256];
    while (fs.getline(line, 256))
    {
      parseLine(line);
    }
    fs.close();
    return true;
  }

  bool MtlTranslator::exportFile(ObjModel* model, const std::string& filename)
  {
    if (!model) return false;
    std::fstream fs(filename.c_str(), std::fstream::out);
    for_each(model->materials.begin(), model->materials.end(),
      fs << bind(&ObjMaterialMap::value_type::second, _1));
    return true;
  }

  bool ObjTranslator::exportFile(ObjModel* model, const std::string& filename)
  {
    if (!model) return false;

    boost::filesystem::path outPath(filename);
    std::fstream fs(filename.c_str(), std::fstream::out);

    fs << "mtllib " << outPath.stem().string() << ".mtl\n";
    fs << *model;
    fs.close();

    MtlTranslator mt;
    outPath.replace_extension(".mtl");
    return mt.exportFile(model, outPath.string());
  }

  int ObjTranslator::parseCluster(std::istream& cluster)
  {
    // Faces can be:
    // a) Vertex only (f V)
    // b) Vertex and UV (f V/T)
    // c) Vertex and Normal (f V//N)
    // d) Vertex, UV and Normal (f V/T/N)
    // We ignore the ordering here because it's dependent on 
    // what vertex data has been parsed.
    char buffer[128];
    int found = 0;
    while (cluster.getline(buffer, 128, '/'))
    {
      uint32_t idx = strtol(buffer, NULL, 10);
      if (idx > 0) 
      {
        _model->faceIndices.push_back(idx-1);
        ++found;
      }
    }
    return found;
  }

  void ObjTranslator::parseLine(char* line)
  {
    char* context ;
    char* token = strtok_r(line, " ", &context);
    if (token == NULL) return;

    if (token[0] == '#') return;

    if (CStringEqual(token, "v"))
    {
      _model->vertices.push_back(parseVec<3>(context));
    }
    else if (CStringEqual(token, "vt"))
    {
      _model->uvs.push_back(parseVec<2>(context));
    }
    else if (CStringEqual(token, "vn"))
    {
      _model->normals.push_back(parseVec<3>(context));
    }
    else if (CStringEqual(token, "g"))
    {
      const char* groupName = strtok_r(NULL, "\n", &context);
      if (!CStringEqual(groupName, "default")) 
      {
        addGroup(groupName);
      }
    }
    else if (CStringEqual(token, "mtllib"))
    {
      mtllib = strtok_r(NULL, "\n", &context);
    }
    else if (CStringEqual(token, "usemtl"))
    {
      addMaterialGroup(strtok_r(NULL, "\n", &context));
    }
    else if (CStringEqual(token, "f"))
    {
      uint32_t indicesAdded = parseFace(context);
      materialGroup()._count += indicesAdded;
      group()._count += indicesAdded;
    }
  } 

  uint32_t ObjTranslator::parseFace(char* context)
  {
      int found = 0;
      char* c = strtok_r(NULL, " ", &context);
      int sizes[3] = {0,0,0};
      uint32_t indicesBefore = _model->faceIndices.size();
      while (c != NULL && found < 3) 
      {
        std::istringstream ss(c);
        sizes[found] = parseCluster(ss);
        c = strtok_r(NULL, " ", &context);
        ++found;
      }
      // Face is a quad, triangulate it now.
      if (c != NULL)
      {
        uint32_t faceSize = sizes[0]+sizes[1]+sizes[2];
        // cluster parser will push the last vertex
        _model->faceIndices.resize(_model->faceIndices.size() + sizes[0] + sizes[1]);
        uint32_t* p0 = &_model->faceIndices[_model->faceIndices.size()] - faceSize -
          sizes[0] - sizes[1];
        uint32_t* p1 = p0 + sizes[0];
        uint32_t* p2 = p1 + sizes[1];
        uint32_t* p3 = p2 + sizes[2];
        uint32_t* p4 = p3 + sizes[0];

        std::copy(p0, p1, p3);
        std::copy(p2, p3, p4);
        std::istringstream ss(c);
        parseCluster(ss);
      }
      uint32_t indicesAdded =  (_model->faceIndices.size() - indicesBefore);
      return indicesAdded;
  }

  bool ObjTranslator::importFile(const std::string& filename, ObjModel* model)
  {
    _model = model;
    boost::filesystem::path objPath(filename);
    std::fstream fs (filename.c_str(), std::fstream::in);
    if (!fs.is_open()) return false;
    char line[256];
    while (fs.getline(line, 256))
    {
      parseLine(line);
    }
    fs.close();

    _model->_name = objPath.stem().string();
    boost::filesystem::path mtlPath(objPath.parent_path() / mtllib);
    MtlTranslator mt;
    if (!mt.importFile(mtlPath.string(), model))
    {
      std::cerr << "error importing mtl " << mtlPath << std::endl;
    }
    return true;
  }


  uint32_t ObjMaterialGroup::numTriangles(const ObjModel& model)const
  {
    return _count / (3 * model.numComponents());
  }

  uint32_t ObjGroup::numTriangles(const ObjModel& model)const
  {
    return _count / (3 * model.numComponents());
  }

  uint32_t objVertexIndex(const ObjModel& m, uint32_t face) { return m.faceIndices[face] + 1; }
  uint32_t objUVIndex(const ObjModel& m, uint32_t face){ return m.faceIndices[face+1] + 1; }
  uint32_t objNormalIndex(const ObjModel& m, uint32_t face) { return m.faceIndices[face+2] + 1; }

  std::ostream& writeFaceVertex(std::ostream& os, const ObjModel& rhs, uint32_t face)
  {
    if (rhs.uvs.empty() && rhs.normals.empty()) // V x
    {
      os << objVertexIndex(rhs, face);
    }
    else if (rhs.uvs.empty()) //, V x//y
    {
      os << objVertexIndex(rhs, face) << "//" << objUVIndex(rhs, face);
    }
    else if (rhs.normals.empty()) // V x/y
    {
      os << objVertexIndex(rhs, face) << "/" << objNormalIndex(rhs, face);
    }
    else
    {
      os << objVertexIndex(rhs, face) << "/" 
        << objUVIndex(rhs, face) << "/"
        << objNormalIndex(rhs, face);
    }

    return os;
  }

  std::ostream& writeFace(std::ostream& os, const ObjModel& rhs, uint32_t offset)
  {
    // Imported faces are always triangles
    os << "f";
    for (uint32_t fv = 0; fv < 3; ++fv)
    {
      os << " ";
      writeFaceVertex(os, rhs, offset + fv * rhs.numComponents());
    }
    os << "\n";

    return os;
  }

  std::ostream& operator<<(std::ostream& os, const ObjModel& rhs)
  {
    if (rhs.vertices.empty()) return os;

    os << "g default\n";

    std::for_each(rhs.vertices.begin(), rhs.vertices.end(),
        os << constant("v ") << _1 << "\n");
    std::for_each(rhs.uvs.begin(), rhs.uvs.end(),
        os << constant("vt ") << _1 << '\n');
    std::for_each(rhs.normals.begin(), rhs.normals.end(),
        os << constant("vn ") << _1 << '\n');

    uint32_t face = 0;
    uint32_t nc = rhs.numComponents();
    for (std::vector<ObjGroup>::const_iterator G = rhs.groups.begin();
        G != rhs.groups.end(); ++G)
    {
      os << "g " << G->_name << "\n";
      for (std::vector<ObjMaterialGroup>::const_iterator MG = G->_materialGroups.begin();
          MG != G->_materialGroups.end(); ++MG)
      {
        os << "usemtl " << MG->_material << "\n";
        for (uint32_t i = 0; i < MG->numTriangles(rhs); ++i) 
        {
          writeFace(os, rhs, face);
          face += nc * 3;
        }
      }
    }

    //    for_each(rhs.materials.begin(), rhs.materials.end(), os << bind(&ObjMaterialMap::value_type::second, _1) << '\n');

    return os;
  }

  std::ostream& operator<<(std::ostream& os, const ObjMaterial& rhs)
  {
    os << "newmtl " << rhs._name << "\n";
    os << "illum 4\n";
    os << "Kd " << rhs.Kd[0] << ' ' << rhs.Kd[1] << ' ' << rhs.Kd[2] << '\n';
    os << "Ka " << rhs.Ka[0] << ' ' << rhs.Ka[1] << ' ' << rhs.Ka[2] << '\n';
    os << "Tf " << rhs.Tf[0] << ' ' << rhs.Tf[1] << ' ' << rhs.Tf[2] << '\n';
    os << "Ni " << rhs.Ni << '\n';
    os << "d " << rhs.d << '\n';
    os << "Ns " << rhs.Ns << '\n';
    os << "Ks " << rhs.Ks[0] << ' ' << rhs.Ks[1] << ' ' << rhs.Ks[2] << '\n';
    if (!rhs.map_Ka.empty()) os << "map_Ka " << rhs.map_Ka << '\n';
    if (!rhs.map_Kd.empty()) os << "map_Kd " << rhs.map_Kd << '\n';
    if (!rhs.map_Ks.empty()) os << "map_Ks " << rhs.map_Ks << '\n';
    return os;
  }

  std::ostream& operator<<(std::ostream& os, const ObjMaterialGroup& rhs)
  {
    os << "mg " << rhs._material << ", " << rhs._count << '\n';
    return os;
  }

  std::ostream& operator<<(std::ostream& os, const ObjGroup& rhs)
  {
    os << "g " << rhs._name << ", " << rhs._count << '\n';
    std::ostream_iterator<ObjMaterialGroup> osi(os, "");
    std::copy(rhs._materialGroups.begin(), rhs._materialGroups.end(), osi);

    return os;
  }
}
