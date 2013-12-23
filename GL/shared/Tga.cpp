// Derived from OpenGL Superbible 5
//
#include "Tga.h"
#include <GL/gl.h>
#include <stdio.h>

#ifdef __BIG_ENDIAN__
///////////////////////////////////////////////////////////
// This function says, "this pointer is a little endian value"
// If the value must be changed it is... otherwise, this
// function is defined away below (on Intel systems for example)
inline void LITTLE_ENDIAN_WORD(void *pWord)
{
  unsigned char *pBytes = (unsigned char *)pWord;
  unsigned char temp;
  
  temp = pBytes[0];
  pBytes[0] = pBytes[1];
  pBytes[1] = temp;
}
#else

// Define them away on little endian systems
#define LITTLE_ENDIAN_WORD 
#define LITTLE_ENDIAN_DWORD 
#endif


// Define targa header. This is only used locally.
#pragma pack(1)
typedef struct
{
  int8_t	identsize;              // Size of ID field that follows header (0)
  int8_t	colorMapType;           // 0 = None, 1 = paletted
  int8_t	imageType;              // 0 = none, 1 = indexed, 2 = rgb, 3 = grey, +8=rle
  unsigned short	colorMapStart;          // First colour map entry
  unsigned short	colorMapLength;         // Number of colors
  unsigned char 	colorMapBits;   // bits per palette entry
  unsigned short	xstart;                 // image x origin
  unsigned short	ystart;                 // image y origin
  unsigned short	width;                  // width in pixels
  unsigned short	height;                 // height in pixels
  int8_t	bits;                   // bits per pixel (8 16, 24, 32)
  int8_t	descriptor;             // image descriptor
} TGAHEADER;
#pragma pack(8)

int imageTypeFromBpp(int bitsPerPixel)
{
  if (bitsPerPixel == 8) return 3;
  if (bitsPerPixel == 24 || bitsPerPixel == 32) return 2;
  return 0;
}
bool gltWriteTGA(const char *szFileName, uint32_t bitsPerPixel, 
                 uint32_t width, uint32_t height, const uint8_t* pixels)
{
  if (imageTypeFromBpp(bitsPerPixel) == 0)
  {
    return false;
  }
  if (pixels == NULL)
  {
    return false;
  }
  
  TGAHEADER tgaHeader;		// TGA file header
  tgaHeader.identsize = 0;
  tgaHeader.colorMapType = 0;
  tgaHeader.imageType = imageTypeFromBpp(bitsPerPixel);
  tgaHeader.colorMapStart = 0;
  tgaHeader.colorMapLength = 0;
  tgaHeader.colorMapBits = 0;
  tgaHeader.xstart = 0;
  tgaHeader.ystart = 0;
  tgaHeader.width = width;
  tgaHeader.height = height;
  tgaHeader.bits = bitsPerPixel;
  tgaHeader.descriptor = 0;
  
  // Do byte swap for big vs little endian
#ifdef __APPLE__
  LITTLE_ENDIAN_WORD(&tgaHeader.colorMapStart);
  LITTLE_ENDIAN_WORD(&tgaHeader.colorMapLength);
  LITTLE_ENDIAN_WORD(&tgaHeader.xstart);
  LITTLE_ENDIAN_WORD(&tgaHeader.ystart);
  LITTLE_ENDIAN_WORD(&tgaHeader.width);
  LITTLE_ENDIAN_WORD(&tgaHeader.height);
#endif
  
  FILE *pFile = fopen(szFileName, "wb");
  if(pFile == NULL)
  {
    return false;
  }
  
  fwrite(&tgaHeader, sizeof(TGAHEADER), 1, pFile);
  fwrite(pixels, width * height * (bitsPerPixel/8), 1, pFile);
  fclose(pFile);
  return true;
}

bool gltLoadTGA(const char *szFileName, 
                uint32_t *iWidth, 
                uint32_t *iHeight, 
                uint32_t *iComponents, 
                uint32_t *eFormat,
                std::vector<uint8_t>& pixels)
{
  
  // Default/Failed values
  *iWidth = 0;
  *iHeight = 0;
  *eFormat = GL_BGR_EXT;
  *iComponents = GL_RGB8;
  
  FILE *pFile = fopen(szFileName, "rb");
  if(pFile == NULL)
    return false;
  
  TGAHEADER tgaHeader;		// TGA file header
  fread(&tgaHeader, 18/* sizeof(TGAHEADER)*/, 1, pFile);
  
  // Do byte swap for big vs little endian
#ifdef __APPLE__
  LITTLE_ENDIAN_WORD(&tgaHeader.colorMapStart);
  LITTLE_ENDIAN_WORD(&tgaHeader.colorMapLength);
  LITTLE_ENDIAN_WORD(&tgaHeader.xstart);
  LITTLE_ENDIAN_WORD(&tgaHeader.ystart);
  LITTLE_ENDIAN_WORD(&tgaHeader.width);
  LITTLE_ENDIAN_WORD(&tgaHeader.height);
#endif
  
  
  // Get width, height, and depth of texture
  *iWidth = tgaHeader.width;
  *iHeight = tgaHeader.height;
  short sDepth = tgaHeader.bits / 8;
  
  if (imageTypeFromBpp(tgaHeader.bits) == 0)
  {
    fclose(pFile);
    return false;    
  }
  
  unsigned long lImageSize = tgaHeader.width * tgaHeader.height * sDepth;
  pixels.resize(lImageSize);
  
  // Read in the bits
  // Check for read error. This should catch RLE or other 
  // weird formats that I don't want to recognize
  if(fread(&pixels[0], lImageSize, 1, pFile) != 1)
  {
    pixels.clear();
    fclose(pFile);
    return false;
  }
  
  // Set OpenGL format expected
  switch(sDepth)
  {
    case 3:     // Most likely case
      *eFormat = GL_BGR_EXT;
      *iComponents = GL_RGB8;
      break;
    case 4:
      *eFormat = GL_BGRA_EXT;
      *iComponents = GL_RGBA8;
      break;
    case 1:
      *eFormat = GL_LUMINANCE;
      *iComponents = GL_LUMINANCE8;
      break;
  };
  
  fclose(pFile);
  return true;
}

