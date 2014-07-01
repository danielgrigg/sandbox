// Derived from OpenGL Superbible 5
//
#include "Tga.h"
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

bool gltWriteTGA(const char *szFileName, int width, int height, 
    const std::vector<uint8_t> &pixels)
{
  const int bitsPerPixel = 8 * pixels.size() / (width * height);

  TGAHEADER tgaHeader;		// TGA file header
  tgaHeader.identsize = 0;
  tgaHeader.colorMapType = 0;
  tgaHeader.imageType = (bitsPerPixel == 24) ? 2 : 3;
  tgaHeader.colorMapStart = 0;
  tgaHeader.colorMapLength = 0;
  tgaHeader.colorMapBits = 0;
  tgaHeader.xstart = 0;
  tgaHeader.ystart = 0;
  tgaHeader.width = width;
  tgaHeader.height = height;
  tgaHeader.bits = bitsPerPixel;
  tgaHeader.descriptor = 0;

#ifdef __APPLE__
  LITTLE_ENDIAN_WORD(&tgaHeader.colorMapStart);
  LITTLE_ENDIAN_WORD(&tgaHeader.colorMapLength);
  LITTLE_ENDIAN_WORD(&tgaHeader.xstart);
  LITTLE_ENDIAN_WORD(&tgaHeader.ystart);
  LITTLE_ENDIAN_WORD(&tgaHeader.width);
  LITTLE_ENDIAN_WORD(&tgaHeader.height);
#endif

  FILE* pFile = fopen(szFileName, "wb");
  if(pFile == NULL) { return false; }
  fwrite(&tgaHeader, sizeof(TGAHEADER), 1, pFile);
  fwrite(&pixels[0], pixels.size(), 1, pFile);
  fclose(pFile);
  return true;
}

