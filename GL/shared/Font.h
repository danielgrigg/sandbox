//
//  Font.h
//  dump_glyphmap
//
//  Created by Daniel Grigg on 3/04/11.
//  Copyright 2011 Daniel Grigg. All rights reserved.
//
#ifndef FONT_H
#define FONT_H

#include <boost/format.hpp>
#include "Image.h"

struct GlyphLayout
{
  GlyphLayout() { m_xOrigin = m_yOrigin = m_xAdvance = m_yAdvance = 0; }
  GlyphLayout(int32_t xOrigin, int32_t yOrigin, 
              int32_t xAdvance, int32_t yAdvance):
  m_xOrigin(xOrigin), 
  m_yOrigin(yOrigin),
  m_xAdvance(xAdvance),
  m_yAdvance(yAdvance)
  {}
  int32_t m_xOrigin;
  int32_t m_yOrigin;
  int32_t m_xAdvance;
  int32_t m_yAdvance;
};

std::ostream &operator<<(std::ostream& os, const GlyphLayout& rhs);

class Glyph
{
public:
  Glyph():m_charCode(0)
  {}

  Glyph(uint32_t charCode, const GlyphLayout& metrics):
    m_charCode(charCode),
    m_metrics(metrics)
  {}
  
  Glyph(uint32_t charCode, const Image<uint8_t>& bitmap, const GlyphLayout& metrics):
  m_charCode(charCode),
  m_bitmap(bitmap),
  m_metrics(metrics)
  {}
  
  uint32_t charCode()const { return m_charCode; }
  const Image<uint8_t>& bitmap()const { return m_bitmap; }
  const GlyphLayout layout()const { return m_metrics; }
  uint32_t area()const { return m_bitmap.width() * m_bitmap.height(); }
  
private:
  uint32_t m_charCode;
  Image<uint8_t> m_bitmap;
  GlyphLayout m_metrics;
};

std::ostream &operator<<(std::ostream& os, const Glyph& rhs);

class FontFace
{
public:
  FontFace(const std::string &name,
           uint16_t size_pts, 
           uint16_t dpi):
  m_name(name),
  m_size_pts(size_pts),
  m_dpi(dpi)
  { }

  bool init(const std::string &contentPath)
  {
    return makeAlphabet(contentPath);
  }

  // Maximum height in pixels.
  const float height()const { return m_height; }
  const uint16_t size_pts()const { return m_size_pts; }
  const uint16_t dpi()const { return m_dpi; }
  const std::string& name()const { return m_name; }
  
  std::string toString()const
  {
    boost::format f = boost::format("%1%.%2%.%3%") % 
    m_name % m_size_pts % m_dpi;
    return f.str();
  }

  const std::vector<Glyph>& alphabet()const { return m_alphabet; }
  const std::vector<int16_t>& kerning()const { return m_hkerning; }
  
private:

  bool makeAlphabet(const std::string& contentPath);
                    
  std::string m_name;
  uint16_t m_size_pts;
  uint16_t m_dpi;
  float m_height;

  std::vector<Glyph> m_alphabet;
  std::vector<int16_t> m_hkerning;
};


#endif
