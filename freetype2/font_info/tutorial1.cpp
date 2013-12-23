#include <iomanip>
#include <iostream>
#include <stdint.h>
#include <string>

#include <ft2build.h>
#include FT_FREETYPE_H

void reportOnFace(FT_Face face)
{
  std::cout << "FamilyName: " << face->family_name << "\n";
  std::cout << "StyleName: " << face->style_name << "\n";
  std::cout << "NumFaces: " << face->num_faces << "\n";
  std::cout << "NumGlyphs: " << face->num_glyphs << "\n";
  std::cout << "NumFixedSizes: " << face->num_fixed_sizes << "\n";
  
  std::cout << "FaceFlags:\n";
  std::cout << std::boolalpha;
  std::cout << "FT_FACE_FLAG_SCALABLE:         " << bool(face->face_flags & FT_FACE_FLAG_SCALABLE) << "\n";
  std::cout << "FT_FACE_FLAG_FIXED_SIZES:      " << bool(face->face_flags & FT_FACE_FLAG_FIXED_SIZES) << "\n";
  std::cout << "FT_FACE_FLAG_FIXED_WIDTH:      " << bool(face->face_flags & FT_FACE_FLAG_FIXED_WIDTH) << "\n";
  std::cout << "FT_FACE_FLAG_SFNT:             " << bool(face->face_flags & FT_FACE_FLAG_SFNT) << "\n";
  std::cout << "FT_FACE_FLAG_HORIZONTAL:       " << bool(face->face_flags & FT_FACE_FLAG_HORIZONTAL) << "\n";
  std::cout << "FT_FACE_FLAG_VERTICAL:         " << bool(face->face_flags & FT_FACE_FLAG_VERTICAL) << "\n";
  std::cout << "FT_FACE_FLAG_KERNING:          " << bool(face->face_flags & FT_FACE_FLAG_KERNING) << "\n";
  std::cout << "FT_FACE_FLAG_MULTIPLE_MASTERS: " << bool(face->face_flags & FT_FACE_FLAG_MULTIPLE_MASTERS) << "\n";
  std::cout << "FT_FACE_FLAG_GLYPH_NAMES:      " << bool(face->face_flags & FT_FACE_FLAG_GLYPH_NAMES) << "\n";
  std::cout << "FT_FACE_FLAG_EXTERNAL_STREAM:  " << bool(face->face_flags & FT_FACE_FLAG_EXTERNAL_STREAM) << "\n";
  std::cout << "FT_FACE_FLAG_HINTER:           " << bool(face->face_flags & FT_FACE_FLAG_HINTER) << "\n";
  std::cout << "FT_FACE_FLAG_CID_KEYED:        " << bool(face->face_flags & FT_FACE_FLAG_CID_KEYED) << "\n";
  std::cout << "FT_FACE_FLAG_TRICKY:           " << bool(face->face_flags & FT_FACE_FLAG_TRICKY) << "\n";
  
  if (face->face_flags & FT_FACE_FLAG_SCALABLE)
  {
    std::cout << "BBox: [[" << face->bbox.xMin << ", " << face->bbox.yMin << 
                      "], [" << face->bbox.xMax << ", " << face->bbox.yMax << 
    "]]\n";
    std::cout << "UnitsPerEM: " << face->units_per_EM << "\n";
    std::cout << "Ascender: " << face->ascender << "\n";
    std::cout << "Descender: " << face->descender << "\n";
    std::cout << "Height: " << face->height << "\n";
    std::cout << "MaxAdvanceWidth: " << face->max_advance_width << "\n";
    std::cout << "MaxAdvanceHeight: " << face->max_advance_height << "\n";
    std::cout << "UnderlinePosition: " << face->underline_position << "\n";
    std::cout << "UnderlineThickness: " << face->underline_thickness << "\n";
  }
  for (int i = 0; i < face->num_charmaps; ++i) {
    FT_CharMapRec* charMap = face->charmaps[i];
    std::cout << "CharMapPlatformId: " << charMap->platform_id << "\n";
    std::cout << "CharMapEncodingId: " << charMap->encoding_id << "\n";
    std::cout << "CharMapEncoding: " << (char)((charMap->encoding >> 24) & 0xFF) <<
      (char)((charMap->encoding >> 16) & 0xFF) <<
      (char)((charMap->encoding >> 8) & 0xFF) <<
      (char)((charMap->encoding) & 0xFF) << "\n";
  }
  
  std::cout << std::noboolalpha;
}


int main(int argc, char **argv)
{
  if (argc < 3)
  {
    std::cerr << "Usage: tutorial1 <font_file_path> <font_size_points> <dpi>" << std::endl;
    return 1;
  }
  const char *fontFile = argv[1];
  const uint32_t fontSize = strtol(argv[2], NULL, 10);
  const uint32_t dpi = strtol(argv[3], NULL, 10);
  FT_Library library;
  uint32_t error = FT_Init_FreeType(&library);
  if (error)
  {
    std::cerr << "Freetype load error" << std::endl;
    return 1;
  }
  
  // Load from a file.  Other functions exist to load from memory or 
  // user-defined streams (think compressed files, network).
  FT_Face     face;      /* handle to face object */
  error = FT_New_Face( library,
                      argv[1],
                      0,
                      &face );
  if ( error == FT_Err_Unknown_File_Format )
  {
    //... the font file could be opened and read, but it appears
    //... that its font format is unsupported
    std::cerr << "Unsupported font format" << std::endl;
    return 1;
  }
  else if ( error )
  {
    // ... another error code means that the font file could not
    // ... be opened or read, or simply that it is broken...
    std::cerr << "Freetype error loading font" << std::endl;
    return 1;
  }
  //std::cout << fontFile <<  " font loaded" << std::endl;
  
  error = FT_Set_Char_Size(face, 0, fontSize*64, 0, dpi);
  if (error)
  {
    std::cerr << "Freetype error set char size\n";
    return 1;
  }
  std::cout << "FontFile: " << fontFile << "@" << fontSize << "pt\n";
  reportOnFace(face);
  
  FT_Done_Face(face);
  FT_Done_FreeType(library);
  
  // We can gather metrics for all fonts like this:
  //  find <fonts_path> -name *.ttf -exec ./tutorial1 {} \;
  return 0;
}

