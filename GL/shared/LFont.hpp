#ifndef LFONT_HPP
#define LFONT_HPP

#include <lexi/Geometry/LTransform.hpp>
#include <lexi/GL/LShader.hpp>
#include "GeometryBatch.h"
#include "FontCache.h"

#include <stdint.h>
#include <string>
#include <vector>

class LFont
{
public:
  LFont(const std::string& family, int size_pts, int dpi, 
        const std::string& contentPath);
  
  //todo - create a 'text' object encapsulating a text buffer and transform.
  void write(int x, int y, const std::string& text);

  void setViewport(int width, int height);
  
private:
  bool makeShader();
  int16_t kerningBetween(uint32_t first, uint32_t second);
  
  uint32_t makeTexture(int width, int height, const std::vector<uint8_t>& pixels);

  bool ready()const { return m_ready; }

  uint32_t m_fontFaceTexture;
  uint32_t m_uniformMVP;
  uint32_t m_uniformFontFaceMap;
  float m_height;
  bool m_ready;
  LShader m_shader;
  GeometryBatch m_batch;
  std::vector<IndexedGlyph> m_index;
  LTransform m_screenProjection;
  std::vector<int16_t> m_kerning;
};

#endif
