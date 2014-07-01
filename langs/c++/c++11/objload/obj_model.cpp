//
//  obj_model.cpp
//  shadowmap
//
//  Created by Daniel Grigg on 21/06/13.
//  Copyright (c) 2013 Sliplane Software. All rights reserved.
//

#include "obj_model.h"
#include <unordered_map>
#include <string>
#include <fstream>
#include <functional>
#include <map>
#include <regex>
#include <iterator>

using std::string;
using std::sregex_token_iterator;
using std::function;

namespace lap {
  
  std::ostream& operator<<(std::ostream& os, const ObjModel& m) {
    
    os << "positions: ";
    std::copy(m._positions.begin(), m._positions.end(), std::ostream_iterator<lap::float3>(os, " "));
    os << "\nnormals: ";
    std::copy(m._normals.begin(), m._normals.end(), std::ostream_iterator<lap::float3>(os, " "));
    os << "\nuvs: ";
    std::copy(m._uvs.begin(), m._uvs.end(), std::ostream_iterator<lap::float2>(os, " "));
    os << "\n";
    
    os << "\nfaces: ";
    std::copy(m._faces.begin(), m._faces.end(), std::ostream_iterator<lap::Face>(os, " "));
    os << "\n";
    return os;
  }
  
  std::ostream& operator<<(std::ostream& os, const Face& rhs) {
    os << rhs.vertex[0] << " " << rhs.vertex[1] << " " << rhs.vertex[2];
    return os;
  }
  
  const string COMMENT="#";
  const string POSITION = "v";
  const string NORMAL = "vn";
  const string UV = "vt";
  const string GROUP = "g";
  const string MTL = "mtllib";
  const string USE_MTL = "usemtl";
  const string FACE = "f";
  
  typedef function<ObjModelPtr&(ObjModelPtr&, sregex_token_iterator args)> ParserFn;
  typedef std::unordered_map<string, ParserFn> ParserMap;
  
  const std::regex ws_re("\\s+");
  const std::regex fv_re("/");
  
  ObjModelPtr& debug_parse(ObjModelPtr& m, sregex_token_iterator iter)
  {
    std::cout << "debug_parse: ";
    std::copy(iter, sregex_token_iterator(), std::ostream_iterator<string>(std::cout, " "));
    std::cout << "\n";
    return m;
  }
  
  ObjModelPtr& identity_parse(ObjModelPtr& m, sregex_token_iterator iter)
  {
    return m;
  }
  
  template <int N>
  vec<float, N> parse_vecf(sregex_token_iterator iter) {
    vec<float, N> v;
    for (auto i = 0; i < N; ++i) {
      if (iter != sregex_token_iterator()) {
        v[i] = std::stof(*iter);
        ++iter;
      }
    }
    return v;
  }
  
  template <int N>
  vec<int, N> parse_veci(sregex_token_iterator iter) {
    vec<int, N> v;
    for (auto i = 0; i < N; ++i) {
      if (iter != sregex_token_iterator()) {
        if (iter->length() > 0) v[i] = std::stoi(*iter);
        ++iter;
      }
    }
    return v;
  }

  
  ObjModelPtr& parse_position(ObjModelPtr& m, sregex_token_iterator iter) {
    m->_positions.push_back(parse_vecf<3>(++iter));
    return m;
  }
  
  ObjModelPtr& parse_uv(ObjModelPtr& m, sregex_token_iterator iter) {
    m->_uvs.push_back(parse_vecf<2>(++iter));
    return m;
  }
  ObjModelPtr& parse_normal(ObjModelPtr& m, sregex_token_iterator iter) {
    m->_normals.push_back(parse_vecf<3>(++iter));
    return m;
  }
  
  int3 parse_face_vertex(string fv_str) {
    auto one_indexed = parse_veci<3>
      (sregex_token_iterator(fv_str.begin(), fv_str.end(), fv_re, -1));
    int3 fv;
    for (auto i = 0; i < 3; ++i) fv[i] = std::max(one_indexed[i] - 1, 0);
    return fv;
  }
  
  Face parse_triangle(string vertices[3]) {
    return {parse_face_vertex(vertices[0]),
            parse_face_vertex(vertices[1]),
            parse_face_vertex(vertices[2])};
  }

  ObjModelPtr& parse_face(ObjModelPtr& m, sregex_token_iterator iter) {
    std::string vs[4];
    ++iter;
    for (int i = 0; i < 4; ++i) {
      if (iter != sregex_token_iterator()) {
        vs[i] = *iter;
        ++iter;
      }
    }
    
    // Add triangle <0, 1 2>
    m->_faces.push_back(parse_triangle(vs));
    
    // Extract quad-faces into two triangles.
    if (!vs[3].empty()) {
      std::string other[3] = { vs[0], vs[2], vs[3]};
      m->_faces.push_back(parse_triangle(other));
    }
    return m;
  }
  
  void parse_line(const ParserMap& parsers, ObjModelPtr& model, const string& line) {
    auto word_iter = std::sregex_token_iterator(line.begin(), line.end(), ws_re, -1);
    if (word_iter == std::sregex_token_iterator() ) return;
    
    const string command = *word_iter;
    
    auto fiter = parsers.find(command);
    if (fiter != parsers.end()) {
      fiter->second(model, word_iter);
    }
    else {
    //  debug_parse(model, word_iter);
    }
  }  
  
  ObjModelPtr obj_model(const std::string path) {
    
    const ParserMap parser_fns =
    { { COMMENT, identity_parse },
      { POSITION, parse_position},
      { NORMAL, parse_normal },
      { UV, parse_uv },
      { FACE, parse_face}};
    
    std::fstream fs (path.c_str(), std::fstream::in);
    if (!fs.is_open()) return ObjModelPtr();
    
    ObjModelPtr model = ObjModelPtr(new ObjModel());
    model->_positions.reserve(512);
    model->_uvs.reserve(512);
    model->_normals.reserve(512);
    model->_faces.reserve(1024);
    
    std::string line;
    while (getline(fs, line)) {
      if (!line.empty()) parse_line(parser_fns, model, line);
    }
    fs.close();
    return model;
  }
}

