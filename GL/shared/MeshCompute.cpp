#include "MeshCompute.h"
#include <tr1/unordered_map>
#include <iterator>
#include <assert.h>
#include <sstream>
#include <algorithm>
#include <iterator>
#include <iostream>
#include <functional>
#include <boost/lexical_cast.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/lambda.hpp>
using namespace boost::lambda;

namespace lmc {

  typedef std::tr1::unordered_map<uint64_t, uint32_t> IndexMap;

  bool Mesh::importFromObjFile(const std::string& filename)
  {
    ObjModel obj;
    ObjTranslator ot;
    if (!ot.importFile(filename, &obj))
    {
      std::cerr << "Error importing " << filename << std::endl;
      return false;
    }
    if (obj.groups.empty()) { std::cerr << "No Obj groups!\n"; return false; }

    return importFromObj(obj);
  }

  bool Mesh::doPosition(const ObjModel& obj)
  {
    is = obj.faceIndices;
    vs.assign(&obj.vertices[0], &obj.vertices[0] + obj.vertices.size());
    return true;
  }

  bool Mesh::doPositionUV(const ObjModel& obj)
  {
    IndexMap indexMap;
    const uint2* objIndices = (uint2*)&obj.faceIndices[0];
    const vec3f* objVS = &obj.vertices[0];
    const vec2f* objUVS = &obj.uvs[0];

    uint64_t yStride = obj.uvs.size() / 2;

    uint32_t largestIndex = 0;
    is.resize(obj.faceIndices.size() / obj.numComponents());
    for (uint32_t index = 0; index < is.size(); ++index)
    {
      const uint2& objIndex = objIndices[index];
      uint64_t key = objIndex.i[0] * yStride + objIndex.i[1];

      std::pair<IndexMap::iterator, bool> pib = 
        indexMap.insert(IndexMap::value_type(key, largestIndex));

      if (pib.second)
      {
        vs.push_back(objVS[objIndex.i[0]]);
        uvs.push_back(objUVS[objIndex.i[1]]);
        is[index] = largestIndex;
        ++largestIndex;
      }
      else
      {
        uint32_t priorIndex = pib.first->second;
        is[index] = priorIndex;
      }
    }
    return true;
  }

  bool Mesh::doPositionNormal(const ObjModel& obj)
  {
    IndexMap indexMap;
    const uint2* objIndices = (uint2*)&obj.faceIndices[0];
    const vec3f* objVS = &obj.vertices[0];
    const vec3f* objNS = &obj.normals[0];
    uint64_t yStride = obj.normals.size() / 3;

    uint32_t largestIndex = 0;
    is.resize(obj.faceIndices.size() / obj.numComponents());
    for (uint32_t index = 0; index < is.size(); ++index)
    {
      const uint2& objIndex = objIndices[index];
      uint64_t key = objIndex.i[0] * yStride + objIndex.i[1];
      std::pair<IndexMap::iterator, bool> pib = 
        indexMap.insert(IndexMap::value_type(key, largestIndex));

      if (pib.second)
      {
        vs.push_back(objVS[objIndex.i[0]]);
        ns.push_back(objNS[objIndex.i[1]]);
        is[index] = largestIndex;
        ++largestIndex;
      }
      else
      {
        uint32_t priorIndex = pib.first->second;
        is[index] = priorIndex;
      }
    }
    return true;
  }

  bool Mesh::doPositionUVNormal(const ObjModel& obj)
  {
    IndexMap indexMap;
    const uint3* objIndices = (uint3*)&obj.faceIndices[0];
    const vec3f* objVS = &obj.vertices[0];
    const vec3f* objNS = &obj.normals[0];
    const uint32_t numObjNS = obj.normals.size() / 3;
    const vec2f* objUVS = &obj.uvs[0];
    const uint32_t numObjUVS = obj.uvs.size() / 2;
    const uint32_t numObjIndices = obj.faceIndices.size() / obj.numComponents(); 

    uint64_t zStride = numObjUVS * numObjNS;
    uint64_t yStride = numObjNS;

    uint32_t largestIndex = 0;
    is.resize(numObjIndices);
    for (uint32_t index = 0; index < numObjIndices; ++index)
    {
      const uint3& objIndex = objIndices[index];
      uint64_t key = objIndex.i[0] * zStride +
        objIndex.i[1] * yStride + 
        objIndex.i[2];

      std::pair<IndexMap::iterator, bool> pib = 
        indexMap.insert(IndexMap::value_type(key, largestIndex));

      if (pib.second)
      {
        vs.push_back(objVS[objIndex.i[0]]);
        uvs.push_back(objUVS[objIndex.i[1]]);
        ns.push_back(objNS[objIndex.i[2]]);
        is[index] = largestIndex;
        ++largestIndex;
      }
      else
      {
        uint32_t priorIndex = pib.first->second;
        is[index] = priorIndex;
      }
    }
    return true;
  }

  bool Mesh::importFromObj(const ObjModel& obj)
  {
    if (obj.vertices.empty()) return false;
    if (obj.uvs.empty() && obj.normals.empty()) doPosition(obj);
    else if (obj.uvs.empty() && !obj.normals.empty()) doPositionNormal(obj);
    else if (!obj.uvs.empty() && obj.normals.empty()) doPositionUV(obj);
    else if (!obj.uvs.empty() && !obj.normals.empty()) doPositionUVNormal(obj);

    _materials = obj.materials;
    importObjGroups(obj);
    return true;
  }

  bool Mesh::importObjMaterialGroups(const ObjGroup& from, MeshGroup& to, uint32_t start,
      uint32_t numComponents)
  {
    to._materialGroups.resize(from._materialGroups.size());
    for (uint32_t materialIndex = 0; materialIndex < from._materialGroups.size();
        ++materialIndex)
    {
      const ObjMaterialGroup& mg = from._materialGroups[materialIndex];
      const uint32_t numIndices = mg._count / numComponents;
      to._materialGroups[materialIndex] = 
        MeshMaterialGroup(start, numIndices, mg._material);
      start += numIndices;
    }
    return true;
  }

  void combineMaterialGroups(MeshGroup& group, 
      const std::vector<uint32_t>* splitIndices, 
      std::vector<uint32_t>* combinedIndices)
  {
    if (group._materialGroups.empty()) return;
    std::vector<MeshMaterialGroup> combined;
    combined.push_back(MeshMaterialGroup(group._start, 0, group._materialGroups[0]._material));
    std::back_insert_iterator<std::vector<uint32_t> > bit(*combinedIndices);
    for (uint32_t i = 0; i < group._materialGroups.size(); ++i)
    {
      const MeshMaterialGroup& mg = group._materialGroups[i];
      if (mg._material == combined.back()._material)
      {
                
        combined.back()._count += mg._count;
      }
      else
      {
         combined.push_back(MeshMaterialGroup(combined.back().end(), mg._count, mg._material));
      }
      std::copy(splitIndices->begin() + mg._start,
          splitIndices->begin() + mg.end(),
          bit);
    }
    group._materialGroups = combined;
  }

  inline bool compareMaterial(const MeshMaterialGroup& a, const MeshMaterialGroup& b)
  {
    return (lexicographical_compare(a._material.begin(), a._material.end(), 
        b._material.begin(), b._material.end()) || 
        a._material == b._material && a._start < b._start);
  }

  void sortByMaterialGroup(MeshGroup& group)
  {
    std::sort(group._materialGroups.begin(), group._materialGroups.end(), 
        compareMaterial);
  }
  
  // Drawing API needs to know min/max elements. 
  // Pity doing something so simple in C++ is so messy :/
  void rangeMaterialGroup(MeshMaterialGroup& mg, 
      const std::vector<uint32_t>* indices)
  {
    mg._minElement = *std::min_element(indices->begin() + mg._start, 
                                        indices->begin() + mg.end());
    mg._maxElement = *std::max_element(indices->begin() + mg._start, 
                                        indices->begin() + mg.end());
  }
  
  // Useless free-function to apply rangeMaterialGroup.
  void rangeGroup(MeshGroup& group, const std::vector<uint32_t>* indices)
  {
    std::for_each(group._materialGroups.begin(), group._materialGroups.end(), 
        bind(rangeMaterialGroup, boost::ref(_1), indices));
  }

  bool Mesh::importObjGroups(const ObjModel& obj)
  {
    _groups.resize(obj.groups.size());

    uint32_t groupStart = 0;
    for (uint32_t groupIndex = 0; groupIndex < obj.groups.size(); ++groupIndex)
    {
      const ObjGroup& G = obj.groups[groupIndex];
      uint32_t numIndices = G._count / obj.numComponents();
      _groups[groupIndex] = MeshGroup(groupStart, numIndices, G._name);

      importObjMaterialGroups(G, _groups[groupIndex], groupStart, 
          obj.numComponents());
      groupStart += numIndices;
    }

    // This block sorts then combines material-group sub-meshes.  However,
    // because the vertices referenced by the sub-meshes may be distributed
    // arbitrarily across the original mesh, the combined groups can have
    // a very wide 'vertex space'.  This is problematic for two reasons,
    // a) the groups have poor spatial locality and b) performing further ops,
    // like chunking into 16-bit index buffers gets hard.
    if (0)
    {
      std::for_each(_groups.begin(), _groups.end(), sortByMaterialGroup);
      std::vector<uint32_t> combinedIndices;
      combinedIndices.reserve(is.size());
      std::for_each(_groups.begin(), _groups.end(), bind(combineMaterialGroups, 
            _1, &is, &combinedIndices));
      is = combinedIndices;
    }

    std::for_each(_groups.begin(), _groups.end(), 
        bind(rangeGroup, boost::ref(_1), &is));
    return true;
  }

  bool isMaterialNameEqual(MeshMaterialGroup& a, const MeshMaterialGroup& b)
  {
    return a._material == b._material;
  }

  typedef std::tr1::unordered_map<uint32_t, uint32_t> Index32Map;

  void Mesh::mergeMaterialGroups(Mesh& result)
  {
    std::vector<MeshMaterialGroup> mgs;
    mgs.assign(beginMaterialGroup(), endMaterialGroup());

    std::vector<MeshMaterialGroup>::iterator iter = mgs.begin();
    uint32_t materialGroupStart = 0;
    result._groups.push_back(MeshGroup(0, 0, "aligned"));
    Index32Map indexMap;
    uint32_t largestIndex = 0;
    while (iter != mgs.end())
    {
      const MeshMaterialGroup& next = *iter;
      std::vector<MeshMaterialGroup>::iterator last = 
        std::partition(iter, mgs.end(), 
            bind(isMaterialNameEqual, boost::ref(_1), boost::ref(next)));
      for(; iter != last; ++iter)
      {
        for (uint32_t idx = iter->_start; idx < iter->end(); ++idx)
        {
          uint32_t ourIndex = is[idx];

          std::pair<Index32Map::iterator, bool> pib = 
            indexMap.insert(Index32Map::value_type(ourIndex, largestIndex));
          if (pib.second)
          {
            result.vs.push_back(vs[ourIndex]);
            if (!ns.empty()) result.ns.push_back(ns[ourIndex]);
            if (!uvs.empty()) result.uvs.push_back(uvs[ourIndex]);
            result.is.push_back(largestIndex);
            ++largestIndex;
          }
          else
          {
            result.is.push_back(pib.first->second);
          }
        }
      }
      result._groups.back()._materialGroups.push_back(
          MeshMaterialGroup(materialGroupStart, 
            result.is.size() - materialGroupStart, 
            next._material));
      materialGroupStart = result.is.size();
      iter = last;
    }
    result._groups[0]._count = result.is.size();
    result._materials = _materials;
    std::for_each(result._groups.begin(), result._groups.end(), 
        bind(rangeGroup, boost::ref(_1), &result.is));
  }

  void assignVertices(const Mesh& from, Mesh& to, uint32_t start, uint32_t end)
  {
    to.vs.assign(from.vs.begin() + start, from.vs.begin() + end);
    if (!from.ns.empty()) 
    {
      to.ns.assign(from.ns.begin() + start, from.ns.begin() + end);
    }
    if (!from.uvs.empty()) 
    {
      to.uvs.assign(from.uvs.begin() + start, from.uvs.begin() + end);
    }
  }

  uint32_t pushVertex(const Mesh& from, Mesh& to, uint32_t index)
  {
    uint32_t vertexIndex = from.is[index];
    to.vs.push_back(from.vs[vertexIndex]);
    if (!from.ns.empty()) to.ns.push_back(from.ns[vertexIndex]);
    if (!from.uvs.empty()) to.uvs.push_back(from.uvs[vertexIndex]);
    return to.vs.size() - 1;
  }

  void partitionVertices(const Mesh& mesh, std::vector<Mesh>& chunks, 
      uint32_t chunkSize)
  {
    chunks.reserve(1 + mesh.vs.size() / chunkSize);
    for (uint32_t chunkStart = 0; chunkStart < mesh.vs.size(); chunkStart += chunkSize)
    {
      chunks.push_back(Mesh());
      std::string chunkName = boost::lexical_cast<std::string>(chunks.size());
      chunks.back()._groups.push_back(MeshGroup(0, 0, chunkName));
      uint32_t copySize = std::min((unsigned long)chunkSize, mesh.vs.size() - chunkStart);
      assignVertices(mesh, chunks.back(), chunkStart, chunkStart + copySize);
    }
  }

  void partitionIndices(const Mesh& mesh, std::vector<Mesh>& chunks, 
      uint32_t chunkSize)
  {
    for (uint32_t idx = 0; idx < mesh.is.size(); idx += 3)
    {
      uint32_t chunkNum[3];
      uint32_t chunkIdx[3];
      for (int i = 0; i < 3; ++i)
      {
        chunkNum[i] = mesh.is[idx+i] / chunkSize;
        chunkIdx[i] = mesh.is[idx+i] % chunkSize;
      }
      chunks[chunkNum[0]].is.push_back(chunkIdx[0]);
      for (int i = 1; i < 3; ++i)
      {
        if (chunkNum[0] != chunkNum[i] ) 
        {
          chunkIdx[i] = pushVertex(mesh, chunks[chunkNum[0]], idx+i);
        }
        chunks[chunkNum[0]].is.push_back(chunkIdx[i]);
      }
    }
  }

  void partitionMaterialGroups(Mesh& mesh, std::vector<Mesh>& chunks, 
      uint32_t chunkSize)
  {
    uint32_t start = 0;
    uint32_t chunkNum = 0;
    for (Mesh::MaterialGroupIterator iter = mesh.beginMaterialGroup(); 
        iter != mesh.endMaterialGroup(); iter++)
    {
      uint32_t remaining = iter->count();
      while (remaining > 0)
      {
        uint32_t count = std::min((unsigned long)remaining, chunks[chunkNum].is.size() - start);
        chunks[chunkNum]._groups[0]._materialGroups.push_back(
            MeshMaterialGroup(start, count, iter->material())) ;
        remaining -= count;
        start += count;
        if (start >= chunks[chunkNum].is.size())
        {
          start = 0;
          ++chunkNum;
        }
      }
    }
  }


  void Mesh::partition(std::vector<Mesh>& chunks, uint32_t chunkSize)
  {
    partitionVertices(*this, chunks, chunkSize);

    partitionIndices(*this, chunks, chunkSize);
  
    partitionMaterialGroups(*this, chunks, chunkSize);

    for (uint32_t idx = 0; idx < chunks.size(); ++idx)
    {
      chunks[idx]._groups[0]._count = chunks[idx].is.size();
      chunks[idx]._materials = _materials;
      rangeGroup(chunks[idx]._groups[0], &chunks[idx].is);
    }
  }

  void Mesh::weld(Mesh& welded)
  {
    welded._materials = _materials;
    welded._groups = _groups;
    welded.is.reserve(is.size());
    for (int i = 0; i < is.size(); ++i)
    {
      for (int v = 0; v < welded.vs.size(); ++v)
      {

      }
    }
  }

#if 0
  template<typename T>
    std::ostream& printArrayJS(std::ostream& os, const T* rhs, uint32_t length)
    {
      os << "[ ";
      for (uint32_t i = 0; i < length; ++i)
      {
        os << rhs[i];
        if (i < length -1) os << ", ";
      }
      os << "] ";
      return os;
    }


  todo Remove once Mesh toJSON is done
    std::string IndexedTriangleBatch::toJSON()const
    {
      std::ostringstream os;
      os << "{\n";
      os << "positions : new Float32Array(";
      printArrayJS(os, (float*)&vs[0], 3 * vs.size());
      os << "),\n";
      if (!ns.empty())
      {
        os << "normals : new Float32Array(";
        printArrayJS(os, (float*)&ns[0], 3 * ns.size());
        os << "),\n";
      }
      if (!uvs.empty())
      {
        os << "uvs : new Float32Array(";
        printArrayJS(os, (float*)&uvs[0], 2 * uvs.size());
        os << "),\n";
      }
      os << "elements : new ";
      if (is.size() > 65536) os << "Uint32Array(";
      else os << "Uint16Array(";
      printArrayJS(os, (uint32_t*)&is[0], is.size());
      os << ")\n";
      os << "}\n";
      return os.str();
    }
#endif

  std::ostream& operator<<(std::ostream& os, const MeshMaterialGroup& rhs)
  {
    os << "material " << rhs._material << ", start " << rhs._start 
      << ", count " << rhs._count << ", minElement " << rhs._minElement
      << ", maxElement " << rhs._maxElement
      << ", range " << rhs.elementRange() << '\n';
    return os;
  }

  std::ostream& operator<<(std::ostream& os, const MeshGroup& rhs)
  {
    os << "name=" << rhs._name 
      << ", start=" << rhs._start << ", count=" << rhs._count << '\n';
    std::ostream_iterator<MeshMaterialGroup> osi(std::cout, "");
    std::copy(rhs._materialGroups.begin(), rhs._materialGroups.end(), osi);
    return os;
  }

  std::ostream& operator<<(std::ostream& os, const Mesh& rhs)
  {
    for (uint32_t i = 0; i < rhs.vs.size(); ++i)
    {
      os << rhs.vs[i] << ",";
      if (!rhs.uvs.empty()) os << rhs.uvs[i];
      os << ",";
      if (!rhs.ns.empty()) os << rhs.ns[i];
      os << '\n';
    }

    for (uint32_t i = 0; i < rhs.is.size(); i+=3)
    {
      os << rhs.is[i] << ", " << rhs.is[i+1] << ", " << rhs.is[i+2] << "\n";
    }

    os << "Groups\n";
    for (uint32_t i = 0; i < rhs._groups.size(); ++i)
    {
      os << rhs._groups[i] << '\n';
    }
    return os;
  }

}
