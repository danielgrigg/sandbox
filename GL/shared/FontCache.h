//
//  Packer.h
//  dump_glyphmap
//
//  Created by Daniel Grigg on 2/04/11.
//  Copyright 2011 Daniel Grigg. All rights reserved.
//

#ifndef FONT_CACHE_H
#define FONT_CACHE_H

#include "Image.h"
#include "Font.h"
#include <iosfwd>

class IndexedGlyph 
{
public:
  IndexedGlyph()
  {
    for (int i = 0; i < 4; ++i) m_uv[i] = 0;
  }

  IndexedGlyph(const GlyphLayout layout):
    m_layout(layout)
  {
    for (int i = 0; i < 4; ++i) m_uv[i] = 0;
  }

  IndexedGlyph(uint16_t u0_, uint16_t v0_, uint16_t u1_, uint16_t v1_, 
               const GlyphLayout layout):
  m_layout(layout)
  {
    m_uv[0] = u0_; m_uv[1] = v0_; m_uv[2] = u1_; m_uv[3] = v1_;
  }
  
  const GlyphLayout& layout()const { return m_layout; }
  const uint16_t u0()const { return m_uv[0]; }
  const uint16_t v0()const { return m_uv[1]; }
  const uint16_t u1()const { return m_uv[2]; }
  const uint16_t v1()const { return m_uv[3]; }
  const uint16_t width()const { return u1() - u0(); }
  const uint16_t height()const { return v1() - v0(); }

private:
  uint16_t m_uv[4];
  GlyphLayout m_layout;
};

std::ostream& operator<<(std::ostream& os, const IndexedGlyph& rhs);

bool makeFontCache(const FontFace& descriptor,
                   Image<uint8_t>& fontCache, 
                   std::vector<IndexedGlyph>& index);

#endif
