#include <iomanip>
#include <iostream>
#include <stdint.h>
#include <string>
#include <vector>
#include "../shared/Tga.h"
#include <ft2build.h>
#include FT_FREETYPE_H
#include <freetype/ftlcdfil.h>

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

  std::cout << "metrics:\n";
  std::cout << "  metric_width " << glyph->metrics.width << "\n";
  std::cout << "  metric_height " << glyph->metrics.height << "\n";
  std::cout << "  horiBearingX " << glyph->metrics.horiBearingX << "\n";
  std::cout << "  horiBearingY " << glyph->metrics.horiBearingY << "\n";
  std::cout << "  horiAdvance " << glyph->metrics.horiAdvance << "\n";
  std::cout << "  vertBearingX " << glyph->metrics.vertBearingX << "\n";
  std::cout << "  vertBearingY " << glyph->metrics.vertBearingY << "\n";
  std::cout << "  vertAdvance " << glyph->metrics.vertAdvance << "\n";

  std::cout << "bitmap_left " << glyph->bitmap_left << "\n";
  std::cout << "bitmap_top " << glyph->bitmap_top << "\n";

  // Assume we're rendered
  std::cout << "Bitmap.rows " << glyph->bitmap.rows << "\n";
  std::cout << "Bitmap.width " << glyph->bitmap.width << "\n";
  std::cout << "Bitmap.pitch " << glyph->bitmap.pitch << "\n";
  std::cout << "Bitmap.num_grays " << glyph->bitmap.num_grays << "\n";
}

void drawGlyph(FT_GlyphSlotRec* glyph, const char c)
{
  std::string outFileName;
  outFileName += c;
  outFileName += ".tga";
  int w = glyph->bitmap.width;
  int h = glyph->bitmap.rows;
  int pitch = glyph->bitmap.pitch;
  std::vector<uint8_t> pixels(w * h, 0xDD);

  for (int y = 0; y < h; ++y)
  {
    for (int x = 0; x < w; ++x)
    {
      uint8_t c = glyph->bitmap.buffer[y*pitch + x];
      if (c == 0) std::cout << ' ';
      else if (c < 127) std::cout << '*';
      else std::cout << '+';
    }
    std::cout << std::endl;
  }
  for (int y = 0; y < h; ++y)
  {
    std::copy(glyph->bitmap.buffer + y * pitch, 
        glyph->bitmap.buffer + y * pitch + w,
        pixels.begin() + (h - y - 1) * w);
  }
  if (g_lcd)
  {
    gltWriteTGA(outFileName.c_str(), w/3, h, pixels);
  }
  else
  {
    // Mac seems to have a bug previewing grayscale tgas.
    std::vector<uint8_t> p2(w*h*3, 0xCC);
    for (int i = 0; i < pixels.size(); ++i)
    {
      p2[i*3] = p2[i*3+1] = p2[i*3+2] = pixels[i];
    }

    if (!gltWriteTGA(outFileName.c_str(), w, h, p2))
    {
      std::cerr << "drawGlyph write error\n";
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

  error = FT_Library_SetLcdFilter(library, FT_LCD_FILTER_DEFAULT);
  if (error)
  {
    std::cerr << "Freetype load error: " << error << std::endl;
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

  const int dpi = 96;
  error = FT_Set_Char_Size(face, fontSize*64, 0, dpi, 0);
  if (error)
  {
    std::cerr << "Freetype error set char size\n";
    return 1;
  }
  std::cout << "FontFile: " << fontFile << "@" << fontSize << "pt\n";

  for (int i = 0; charCodes[i] != 0; ++i) 
  { 
    std::cout << "'" << charCodes[i] << "'\n";
    //if (legacyPath(face, charCodes[i])) return 1;
    if (newPath(face, charCodes[i])) return 1; 
    reportGlyphMetrics(face->glyph);
    drawGlyph(face->glyph, charCodes[i]);
  }  
  FT_Done_Face(face);
  FT_Done_FreeType(library);

  return 0;
}

