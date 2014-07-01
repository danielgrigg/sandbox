#include "Tga.h"
#include <stdio.h>

#ifdef __BIG_ENDIAN__
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
  int8_t	identsize;              
  int8_t	colorMapType;          
  int8_t	imageType;            
  unsigned short	colorMapStart;
  unsigned short	colorMapLength;
  unsigned char 	colorMapBits; 
  unsigned short	xstart;      
  unsigned short	ystart;     
  unsigned short	width;     
  unsigned short	height;   
  int8_t	bits;            
  int8_t	descriptor;     
} TGAHEADER;
#pragma pack(8)

int imageTypeFromBpp(int bitsPerPixel)
{
  if (bitsPerPixel == 8) return 3;
  if (bitsPerPixel == 24 || bitsPerPixel == 32) return 2;
  return 0;
}
bool TgaWrite(const char *szFileName, uint32_t bitsPerPixel, 
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

