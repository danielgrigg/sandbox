#ifndef MESH_COMPUTE_H
#define MESH_COMPUTE_H

#include <vector>
#include <iostream>
#include "ObjModel.h"

namespace lmc {

class Mesh;
  
struct MeshMaterialGroup
{
  MeshMaterialGroup(){}

  MeshMaterialGroup(uint32_t start, uint32_t count, const std::string& material,
      uint32_t minElement = 0, uint32_t maxElement = ~0):
    _start(start),
    _count(count),
    _minElement(minElement),
    _maxElement(maxElement),
    _material(material)
  {}

  uint32_t end()const { return _start + _count; }  
  uint32_t elementRange()const { return _maxElement - _minElement; }
  const std::string& material()const { return _material; }
  const uint32_t count()const { return _count; }
  const uint32_t start()const { return _start; }

  uint32_t _start;
  uint32_t _count;
  uint32_t _minElement;
  uint32_t _maxElement;
  std::string _material;
};

std::ostream& operator<<(std::ostream& os, const MeshMaterialGroup& rhs);

struct MeshGroup
{
  MeshGroup()
  {}

  MeshGroup(uint32_t start, uint32_t count, 
      const std::string& name):
    _start(start),
    _count(count),
    _name(name)
  {}

  uint32_t end()const { return _start + _count; }
  uint32_t _start;
  uint32_t _count;
  std::string _name;
  std::vector<MeshMaterialGroup> _materialGroups;
};

std::ostream& operator<<(std::ostream& os, const MeshGroup& rhs);

struct Mesh
{
  
  class MaterialGroupIterator : 
  public std::iterator<std::input_iterator_tag, int>
  {
    Mesh* _mesh;
    uint32_t _gIdx;
    uint32_t _mgIdx;
  public:
    MaterialGroupIterator(Mesh* mesh, uint32_t groupIndex)
    {
      _mesh = mesh;
      _gIdx  = groupIndex;
      _mgIdx = 0;
    }
    MaterialGroupIterator(const MaterialGroupIterator& mit) : 
    _mesh(mit._mesh),
    _gIdx(mit._gIdx),
    _mgIdx(0)
    {}
    
    MaterialGroupIterator& operator++() 
    {
      _mgIdx++;
      if (_mgIdx >= _mesh->_groups[_gIdx]._materialGroups.size())
      {
        _mgIdx = 0;
        _gIdx++;
      }

      return *this;
    }
    MaterialGroupIterator operator++(int) 
    {MaterialGroupIterator tmp(*this); operator++(); return tmp;}
    bool operator==(const MaterialGroupIterator& rhs) 
    {
      return _mesh == rhs._mesh && _gIdx == rhs._gIdx &&
        _mgIdx == rhs._mgIdx;
    }
    bool operator!=(const MaterialGroupIterator& rhs) 
    {
      return !(*this == rhs);
    }
    MeshMaterialGroup& operator*() 
    {
      return _mesh->_groups[_gIdx]._materialGroups[_mgIdx];
    }
    MeshMaterialGroup* operator->() 
    {
      return &(_mesh->_groups[_gIdx]._materialGroups[_mgIdx]);
    }
  };

  std::vector<vec3f> vs;
  std::vector<vec3f> ns;
  std::vector<vec2f> uvs;
  std::vector<uint32_t> is;
  std::vector<MeshGroup> _groups;
  ObjMaterialMap _materials;

  bool importFromObjFile(const std::string& filename);
  bool importFromObj(const ObjModel& model);

  void partition(std::vector<Mesh>& chunks, uint32_t chunkSize);

  void weld(Mesh& welded);

  // Merge duplicate material-group sub-meshes. This destroys
  // existing groups.
  void mergeMaterialGroups(Mesh& result);

  MaterialGroupIterator beginMaterialGroup()
  {
    return MaterialGroupIterator(this, 0); 
  }
  MaterialGroupIterator endMaterialGroup()
  { 
    return MaterialGroupIterator(this, _groups.size());
  }

  private:

  bool doPositionUVNormal(const ObjModel& obj);
  bool doPositionUV(const ObjModel& obj);
  bool doPositionNormal(const ObjModel& obj);
  bool doPosition(const ObjModel& obj);
  bool importObjGroups(const ObjModel& obj);
  bool importObjMaterialGroups(const ObjGroup& from, MeshGroup& to, 
      uint32_t start, uint32_t numComponents);
};

std::ostream& operator<<(std::ostream& os, const Mesh& rhs);

struct uint3 { uint32_t i[3]; };
struct uint2 { uint32_t i[2]; };
struct Face { uint3 v[3]; };

}
#endif
