//
//  Font.cpp
//  dump_glyphmap
//
//  Created by Daniel Grigg on 3/04/11.
//  Copyright 2011 Daniel Grigg. All rights reserved.
//

#include "Font.h"
#include "Image.h"
#include <ft2build.h>
#include FT_FREETYPE_H
#include <freetype/ftlcdfil.h>
#include <tr1/memory>
#include <boost/bind.hpp>
#include <iostream>
#include <numeric>

const uint32_t kCharBegin = 0;
const uint32_t kCharEnd = 256;

float fixed16_16ToFloat(uint32_t from)
{
  const float k = 1.0f / 65536.0f;
  return from * k;
}

float fixed26_6ToFloat(uint32_t from)
{
  const float k = 1.0f / 64.0f;
  return from * k;
}

Image<uint8_t> makeImageFromGlyphBitmap(const uint8_t* bitmap, 
                                  int width, int height, int pitch)
{
  Image<uint8_t> image(width, height);
  for (int y = 0; y < height; ++y)
  {
    for (int x = 0; x < width; ++x)
    {
      image.setPixel(x, height - y - 1, bitmap[y * pitch + x]);
    }
  }
  return image;
}

Glyph makeGlyph(FT_Face face, int unic)
{
  unsigned long glyphIndex = FT_Get_Char_Index(face, unic);
  
  FT_Load_Glyph(face, glyphIndex, FT_LOAD_RENDER);
  if (face->glyph->format != FT_GLYPH_FORMAT_BITMAP)
  {
    FT_Render_Glyph(face->glyph, FT_RENDER_MODE_NORMAL);
  }
  const FT_GlyphSlotRec* slot = face->glyph;

  GlyphLayout layout(slot->bitmap_left, slot->bitmap_top, 
      slot->advance.x >> 6, slot->advance.y >> 6);
  if (glyphIndex == 0)
  {
    return Glyph(unic, layout);
  }
  return Glyph(unic, 
              makeImageFromGlyphBitmap(slot->bitmap.buffer, 
                                       slot->bitmap.width, 
                                       slot->bitmap.rows, 
                                       slot->bitmap.pitch),
              layout);
}

#if 0
void makeKerningTable(FontFace& fontFace, FT_FACE face);
{
  fontFace.hkerning.resize(kCharEnd*kCharEnd);
  for (int y = 0; y < kCharEnd; ++y)
  {
    unsigned long iIndex = FT_Get_Char_Index(face, y);
    for (int x = 0; x < kCharEnd; ++x)
    {
      unsigned long xIndex = FT_Get_Char_Index(face, x);
      FT_Vector kerning;
      FT_Get_Kerning(face, y, x, FT_KERNING_DEFAULT, &kerning);
      m_hkerning[y*kCharEnd+x] = kerning.x >> 6;
    }
  }
}
#endif

bool openFreeType(const FontFace& fontFace, const std::string& path, 
    FT_Library& library, FT_Face& face)
{
  uint32_t error = FT_Init_FreeType(&library);
  if (error)
  {
    std::cerr << "Freetype initialisation error" << std::endl;
    return false;
  }

  const std::string fontFile = path + fontFace.name() + ".ttf";
  error = FT_New_Face(library, fontFile.c_str(), 0, &face );
  if ( error )
  {
    std::cerr << "Freetype error, font not found." << std::endl;
    FT_Done_FreeType(library);
    return false;
  }

  FT_Set_Char_Size(face, fontFace.size_pts()*64, 0, fontFace.dpi(), 0);
  return true;
}

bool closeFreeType(FT_Library& library, FT_Face& face)
{
  FT_Done_Face(face);
  FT_Done_FreeType(library);
  return true;
}

int maxBitmapHeight(int maxHeight, const Glyph& r)
{
  return std::max(maxHeight, r.bitmap().height());
}

bool FontFace::makeAlphabet(const std::string& contentPath)
{
  FT_Library library;
  FT_Face face;
  if (!openFreeType(*this, contentPath, library, face)) return false;
  m_alphabet.clear();
  m_alphabet.reserve(256);
  for (int i = 0; i < 256; ++i) { m_alphabet.push_back(makeGlyph(face, i)); }

  // FreeType seems to kinda messed for vertical advances of horizontal layouts,
  // but we likely want to scale the font regardless..
  //  uint32_t heightPts = ((face->height + 63) & -64) >> 6;
  // m_height = heightPts * m_dpi / 72.0f;
  m_height =  std::accumulate(m_alphabet.begin(), m_alphabet.end(), 0, maxBitmapHeight);

#if 0
  makeKerningTable(face);
#endif
  closeFreeType(library, face);
  return true;
}

std::ostream &operator<<(std::ostream& os, const Glyph& rhs)
{
  os << "([" << rhs.bitmap().width() << ", " << rhs.bitmap().height() <<
    "], " << rhs.layout() << ")";
  return os;
}

std::ostream &operator<<(std::ostream& os, const GlyphLayout& rhs)
{
  os << "[" << rhs.m_xOrigin << ", " << rhs.m_yOrigin << ", " << 
    rhs.m_xAdvance << ", " << rhs.m_yAdvance << "]";
  return os;
}

