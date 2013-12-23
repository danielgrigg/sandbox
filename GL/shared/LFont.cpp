#include "LFont.hpp"

LFont::LFont(const std::string& family, int size_pts, int dpi, 
    const std::string& contentPath)
{
  m_fontFaceTexture = 0;
  m_ready = false;

  Image<uint8_t> fontCache;
  FontFace descriptor(family, size_pts, dpi);
  if (!descriptor.init(contentPath))
  {
    std::cerr << "Error creating font" << std::endl;
    return;
  }
  m_height = descriptor.height();
  std::cout << "height: " << m_height << std::endl;
  if (!makeFontCache(descriptor, fontCache, m_index))
  {
    std::cerr << "Error creating font cache" << std::endl;
    return;
  }
  m_fontFaceTexture = makeTexture(fontCache.width(), fontCache.height(), 
      fontCache.pixels());
  if (!makeShader())
  {
    std::cerr << "Error creating font shader" << std::endl;
    return;
  }
  m_batch.init(GL_TRIANGLE_STRIP, "text");
  m_ready = true;
#if 0
  m_kerning = descriptor.kerning();
#endif
}

int16_t LFont::kerningBetween(uint32_t first, uint32_t second)
{
  return m_kerning[first * 256 + second];
}

//todo - create a 'text' object encapsulating a text buffer and transform.
void LFont::write(int x, int y, const std::string& text)
{
  if (!ready()) return;

  glUseProgram(m_shader.program());
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_RECTANGLE, m_fontFaceTexture);

  LTransform M = LTranslate(x,y,0);
  LTransform MVP = m_screenProjection * M;
  glUniformMatrix4fv(m_uniformMVP, 1, GL_FALSE, MVP.array());
  glUniform1i(m_uniformFontFaceMap, 0);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glDisable(GL_DEPTH_TEST);

  int32_t xPen = 0;
  int32_t yPen = 0;
  int n = 0;
  m_batch.setDirty();
  m_batch.ps.clear();
  m_batch.uvs[0].clear();
  int32_t dy = m_height;
  for (uint32_t i = 0; i < text.size(); ++i)
  {
    // 1 2 3 4 4,  5 5 6 7 8 8, 9 9 10 11 12,
    const char c = text[i];
    if (c == '\n')
    {
      yPen -= dy;
      xPen = 0;
      continue;
    }
#if 0
    if (i > 0)
    {
      xPen += kerningBetween(text[i-1], c);
    }
#endif
    IndexedGlyph& glyph = m_index[c]; 
    int x = xPen + glyph.layout().m_xOrigin;
    int y = yPen + (glyph.layout().m_yOrigin - glyph.height());
    if (i != 0)
    {
      m_batch.ps.push_back(LPoint(x, y+glyph.height(), 0));
      m_batch.uvs[0].push_back(UV(glyph.u0(), glyph.v1()));
    }

    m_batch.ps.push_back(LPoint(x, y+glyph.height(), 0));
    m_batch.uvs[0].push_back(UV(glyph.u0(), glyph.v1()));

    m_batch.ps.push_back(LPoint(x, y, 0));
    m_batch.uvs[0].push_back(UV(glyph.u0(), glyph.v0()));

    m_batch.ps.push_back(LPoint(x+glyph.width(), y+glyph.height(), 0));
    m_batch.uvs[0].push_back(UV(glyph.u1(), glyph.v1()));

    m_batch.ps.push_back(LPoint(x+glyph.width(), y, 0));
    m_batch.uvs[0].push_back(UV(glyph.u1(), glyph.v0()));

    if (i != text.size()-1)
    {
      m_batch.ps.push_back(LPoint(x+glyph.width(), y, 0));
      m_batch.uvs[0].push_back(UV(glyph.u1(), glyph.v0()));
    }

    xPen += glyph.layout().m_xAdvance;
    yPen += glyph.layout().m_yAdvance;
  }

  if (false)
  {
    LColor color(1.0f, 1.0f, 1.0f, 1.0f);
    m_batch.cs.assign(m_batch.ps.size(), color);
  }
  m_batch.bind();
  m_batch.draw();

  glEnable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);

}

void LFont::setViewport(int width, int height)
{
  m_screenProjection = LOrtho(0, width, 0, height);
}

bool LFont::makeShader()
{
  vector<string> sources;
  sources.push_back("../shared/LFont.fp");
  sources.push_back("../shared/LFont.vp");

  vector<std::pair<string,int> > attributes;
  attributes.push_back(std::make_pair("vVertexPos", (int)GeometryBatch::kPosOffset));
  attributes.push_back(std::make_pair("vTexCoord", (int)GeometryBatch::kUV0Offset));
  //attributes.push_back(std::make_pair("vColor", (int)GeometryBatch::kColorOffset));
  m_shader.setSources(sources, attributes);
  if (!m_shader.build()) return false;
  m_uniformMVP = glGetUniformLocation(m_shader.program(), "mvpMatrix");
  m_uniformFontFaceMap = glGetUniformLocation(m_shader.program(), "fontFaceMap");
  return true;
}

GLuint LFont::makeTexture(int width, 
    int height, 
    const std::vector<uint8_t>& pixels)
{
  GLuint texture;
  glGenTextures(1, &texture);  
  glBindTexture(GL_TEXTURE_RECTANGLE, texture);

  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glTexImage2D(GL_TEXTURE_RECTANGLE, 0, GL_LUMINANCE8, width, height, 0, 
      GL_LUMINANCE, GL_UNSIGNED_BYTE, &pixels[0]);
  return texture;
}

