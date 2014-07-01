#include <iomanip>
#include <iostream>
#include <stdint.h>
#include <string>
#include <vector>
#include "../shared/Tga.h"

#include <ft2build.h>
#include FT_FREETYPE_H

bool g_lcd = false;

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

void reportGlyphMetrics(FT_GlyphSlotRec* glyph)
{
  std::cout << "linearHoriAdvance " << 
  fixed16_16ToFloat(glyph->linearHoriAdvance) << "\n";
  std::cout << "linearVertAdvance " << 
  fixed16_16ToFloat(glyph->linearVertAdvance) << "\n";
  std::cout << "advance (" << fixed26_6ToFloat(glyph->advance.x) << ", " <<
    fixed26_6ToFloat(glyph->advance.y) << ")\n";
  //  std::cout << "format " << (uint32_t)glyph->format << "\n";
  std::cout << "bitmap_left " << glyph->bitmap_left << "\n";
  std::cout << "bitmap_top " << glyph->bitmap_top << "\n";
  
  // Assume we're rendered
  std::cout << "Bitmap.rows " << glyph->bitmap.rows << "\n";
  std::cout << "Bitmap.width " << glyph->bitmap.width << "\n";
  std::cout << "Bitmap.pitch " << glyph->bitmap.pitch << "\n";
  std::cout << "Bitmap.num_grays " << glyph->bitmap.num_grays << "\n";
}

struct GreyCanvas
{
  std::vector<uint8_t> m_pixels;
  int m_width;
  int m_height;

  GreyCanvas(int w, int h)
  {
    m_width = w;
    m_height = h;

    m_pixels.assign(w*h, 0);
  }

  void setPixel(int x, int y, uint8_t l)
  {
    if (x < 0 || x >= m_width || y < 0 || y >= m_height) return;
    m_pixels[y * m_width + x] |= l;
  }

  void save()
  {
    std::vector<uint8_t> flippedPixels(m_width * m_height);

    for (int y = 0; y < m_height; ++y)
    {
      std::copy(m_pixels.begin() + y * m_width, 
          m_pixels.begin() + (y+1) * m_width,
          flippedPixels.begin() + (m_height - y - 1) * m_width);
    }

    std::vector<uint8_t> outPixels;

    // CD    CCCDDD
    // AB    AAABBB
    if (g_lcd)
    {
      int w = m_width / 3;
      int h = m_height;
      outPixels.assign(flippedPixels.begin(), flippedPixels.end()); 
      gltWriteTGA("line.tga", w, h, outPixels);
    }
    else
    {
      outPixels.assign(3 * m_width * m_height,0);
      for (int i = 0; i < flippedPixels.size(); i++)
      {
        outPixels[i*3] = flippedPixels[i+0];
        outPixels[i*3+1] = flippedPixels[i+0];
        outPixels[i*3+2] = flippedPixels[i+0];
      }
      gltWriteTGA("line.tga", m_width, m_height, outPixels);
    }
  }
};

void drawGlyph(FT_GlyphSlotRec* glyph, 
    const char c,
    int xPen,
    int yPen,
    GreyCanvas &canvas)
{
  int gw = glyph->bitmap.width;
  int gh = glyph->bitmap.rows;
  int pitch = glyph->bitmap.pitch;

  std::cout << "glyph(" << xPen << "," << yPen << "," <<
    gw << "," << gh << ")\n";


  for (int y = 0; y < gh; ++y)
  {
    for (int x = 0; x < gw; ++x)
    {
      uint8_t c = glyph->bitmap.buffer[y * pitch + x];
      if (c == 0) std::cout << ' ';
      else if (c < 127) std::cout << '*';
      else std::cout << '+';
    }
    std::cout << std::endl;
  }

  for (int y = 0; y < gh; ++y)
  {
    for (int x = 0; x < gw; ++x)
    {
      canvas.setPixel(xPen + x, yPen + y, 
          glyph->bitmap.buffer[y * pitch + x]);
    }
  }
}

int legacyPath(FT_Face face, uint32_t unic)
{
  unsigned long glyphIndex = FT_Get_Char_Index(face, unic);
  if (glyphIndex == 0)
  {
    std::cerr << "Undefined character code '" << unic << "'\n";
    return 1;
  }
  uint32_t error = FT_Load_Glyph(face, glyphIndex, FT_LOAD_TARGET_LCD);
  if (error)
  {
    std::cerr << "Freetype load glyph error\n";
    return 1;
  }
  if (face->glyph->format != FT_GLYPH_FORMAT_BITMAP)
  {
    // FT_RENDER_MODE_LCD / FT_RENDER_MODEL_LCD_V
    if (FT_Render_Glyph(face->glyph, FT_RENDER_MODE_LCD))
    {
      std::cerr << "Freetype render glyph error\n";
      return 1;
    }
  }
  return 0;
}



int newPath(FT_Face face, uint32_t unic)
{
  uint32_t flags = g_lcd ? FT_LOAD_RENDER | FT_LOAD_TARGET_LCD  :
    FT_LOAD_RENDER;
  uint32_t error = FT_Load_Char(face, unic, flags );
  if (error) 
  {
    std::cout << "Freetype load char error\n";
    return 1;
  }
  return 0;
}

int main(int argc, char **argv)
{
  if (argc < 4)
  {
    std::cerr << 
      "Usage: tutorial1 <font_file_path> <font_size_points> <charCodes>" 
      << std::endl;
    return 1;
  }
  const char *fontFile = argv[1];
  const uint32_t fontSize = strtol(argv[2], NULL, 10);
  const char *charCodes = argv[3];
  FT_Library library;
  uint32_t error = FT_Init_FreeType(&library);
  if (error)
  {
    std::cerr << "Freetype load error" << std::endl;
    return 1;
  }

  FT_Face     face;      /* handle to face object */
  error = FT_New_Face( library,
      argv[1],
      0,
      &face );
  if ( error == FT_Err_Unknown_File_Format )
  {
    std::cerr << "Unsupported font format" << std::endl;
    return 1;
  }
  else if ( error )
  {
    std::cerr << "Freetype error loading font" << std::endl;
    return 1;
  }

  int dpi = 96;
  error = FT_Set_Char_Size(face, fontSize*64, 0, dpi, 0);
  if (error)
  {
    std::cerr << "Freetype error set char size\n";
    return 1;
  }
  std::cout << "FontFile: " << fontFile << "@" << fontSize << "pt\n";

  FT_GlyphSlotRec* slot = face->glyph;
  GreyCanvas canvas(512, 512);

  FT_Vector pen;
  pen.x = 200 * 64;
  pen.y = (512 - 200) * 64;
  for (int i = 0; charCodes[i] != 0; ++i) 
  { 
    std::cout << "'" << charCodes[i] << "'\n";

    FT_Set_Transform(face, NULL, &pen);

    //    if (legacyPath(face, charCodes[i])) return 1;
    if (newPath(face, charCodes[i])) return 1;

    reportGlyphMetrics(slot);
    std::cout << "pen(" << pen.x << ", " << pen.y << ")\n";
    drawGlyph(slot, charCodes[i], 
        slot->bitmap_left, 
        512 - slot->bitmap_top, canvas);
    pen.x += slot->advance.x;
    pen.y += slot->advance.y;
  }  
  canvas.save();

  FT_Done_Face(face);
  FT_Done_FreeType(library);

  return 0;
}

