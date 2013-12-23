#ifndef OBJ_IMPORT_HPP
#define OBJ_IMPORT_HPP

#include <vector>
#include <iosfwd>
#include <stdint.h>
#include <string>
#include <tr1/unordered_map>
#include "MeshMath.h"

//
namespace lmc {

  class ObjModel;


  struct ObjMaterial
  {
    ObjMaterial()
    {
      init();
    }
    ObjMaterial(const std::string& name):_name(name){ init(); }

    void init()
    {
      Ni = 0.0f;
      d = 0.0f;
      Ns = 0.0f;
    }

    // For simplicity we store copies of all standard MTL values
    // and maps. Maps, if given, override non-maps.
    vec3f Kd; // Diffuse-coefficient (RGB 0-1)
    float Ni; // Refraction index (0.001 - 10)
    vec3f Ka; // Ambient-coefficient (RGB 0-1)
    float d; // Dissolve (0-1)
    vec3f Tf; // Transmission-filter (RGB 0-1)
    float Ns; // Specular-exponent (0 - 1000)
    vec3f Ks; // Specular-coefficient (RGB) 0-1
    // illum unsupported
    std::string _name;
    std::string map_Ka;
    std::string map_Kd;
    std::string map_Ks;
    // todo - other maps
  };

  std::ostream& operator<<(std::ostream& os, const ObjMaterial& rhs);

  struct ObjMaterialGroup
  {
    ObjMaterialGroup(const char* material):
      _material(material),
      _count(0)
    {}

    std::string _material;
    uint32_t _count;
    uint32_t numTriangles(const ObjModel& model)const;
  };

  std::ostream& operator<<(std::ostream& os, const ObjMaterialGroup& rhs);

  struct ObjGroup
  {
    ObjGroup(const char* name):
      _count(0),
      _name(name)
    {}
    uint32_t _count;
    std::string _name;
    std::vector<ObjMaterialGroup> _materialGroups;
    uint32_t numTriangles(const ObjModel& model)const;
  };

  std::ostream& operator<<(std::ostream& os, const ObjGroup& rhs);
  typedef std::tr1::unordered_map<std::string, ObjMaterial> ObjMaterialMap;

  struct ObjModel
  {
    std::vector<vec3f> vertices;
    std::vector<vec2f> uvs;
    std::vector<vec3f> normals;
    std::vector<uint32_t> faceIndices;
    std::vector<ObjGroup> groups;
    std::string _name;
    ObjMaterialMap materials;

    const std::string& name()const { return _name; }

    uint32_t numComponents()const
    {
      uint32_t components = 1;
      if (!normals.empty()) components += 1;
      if (!uvs.empty()) components += 1;
      return components;
    }

    private:
  };
  std::ostream& operator<<(std::ostream& os, const ObjModel& rhs);

  class MtlTranslator
  {
    public:
      bool importFile(const std::string& filename, ObjModel* model);
      bool exportFile(ObjModel* model, const std::string& filename);
    private:
      void parseLine(char* line);

      ObjModel* _model;
      std::string _working;
      ObjMaterial& working() { return _model->materials[_working]; }
  };

  class ObjTranslator
  {
    public:
      bool importFile(const std::string& filename, ObjModel* model);
      bool exportFile(ObjModel* model, const std::string& filename);

    private:
      int parseCluster(std::istream& cluster);
      void parseLine(char* line);
      uint32_t parseFace(char* context);
      ObjModel* _model;
      std::string mtllib; // Obj-format token for a Obj-material file.

      ObjGroup& group() { return _model->groups.back(); }
      ObjMaterialGroup& materialGroup() 
      { 
        return _model->groups.back()._materialGroups.back(); 
      }

      void addGroup(const char* name)
      {
        _model->groups.push_back(ObjGroup(name));
      }

      void addMaterialGroup(const char* material)
      {
        group()._materialGroups.push_back(ObjMaterialGroup(material));
      }
  };

}

#endif
