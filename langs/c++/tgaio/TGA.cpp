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

////////////////////////////////////////////////////////////////////
// Capture the current viewport and save it as a targa file.
// Be sure and call SwapBuffers for double buffered contexts or
// glFinish for single buffered contexts before calling this function.
// Returns 0 if an error occurs, or 1 on success.
bool gltWriteTGA(const char *szFileName, int width, int height, 
    const std::vector<int8_t> &pixels)
{
  FILE *pFile;                // File pointer
  TGAHEADER tgaHeader;		// TGA file header
  unsigned long lImageSize;   // Size in bytes of image
  int8_t	*pBits = NULL;      // Pointer to bits
  int32_t iViewport[4];         // Viewport in pixels
  uint32_t lastBuffer;          // Storage for the current read buffer setting
#if 0
  // Get the viewport dimensions
  glGetIntegerv(GL_VIEWPORT, iViewport);

  // How big is the image going to be (targas are tightly packed)
  lImageSize = iViewport[2] * 3 * iViewport[3];	

  // Allocate block. If this doesn't work, go home
  pBits = (int8_t *)malloc(lImageSize);
  if(pBits == NULL)
    return false;

  // Read bits from color buffer
  glPixelStorei(GL_PACK_ALIGNMENT, 1);
  glPixelStorei(GL_PACK_ROW_LENGTH, 0);
  glPixelStorei(GL_PACK_SKIP_ROWS, 0);
  glPixelStorei(GL_PACK_SKIP_PIXELS, 0);

  // Get the current read buffer setting and save it. Switch to
  // the front buffer and do the read operation. Finally, restore
  // the read buffer state
  glGetIntegerv(GL_READ_BUFFER, (int32_t *)&lastBuffer);
  glReadBuffer(GL_FRONT);
  glReadPixels(0, 0, iViewport[2], iViewport[3], GL_BGR_EXT, GL_UNSIGNED_BYTE, pBits);
  glReadBuffer(lastBuffer);
#endif

  // Initialize the Targa header
  tgaHeader.identsize = 0;
  tgaHeader.colorMapType = 0;
  tgaHeader.imageType = 2;
  tgaHeader.colorMapStart = 0;
  tgaHeader.colorMapLength = 0;
  tgaHeader.colorMapBits = 0;
  tgaHeader.xstart = 0;
  tgaHeader.ystart = 0;
  tgaHeader.width = width;
  tgaHeader.height = height;
  tgaHeader.bits = 24;
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

  // Attempt to open the file
  pFile = fopen(szFileName, "wb");
  if(pFile == NULL)
  {
    return false;
  }

  // Write the header
  fwrite(&tgaHeader, sizeof(TGAHEADER), 1, pFile);

  // Write the image data
  fwrite(&pixels[0], lImageSize, 1, pFile);

  // Free temporary buffer and close the file
  fclose(pFile);

  // Success!
  return true;
}


////////////////////////////////////////////////////////////////////
// Allocate memory and load targa bits. Returns pointer to new buffer,
// height, and width of texture, and the OpenGL format of data.
// Call free() on buffer when finished!
// This only works on pretty vanilla targas... 8, 24, or 32 bit color
// only, no palettes, no RLE encoding.
#include <vector>

bool gltLoadTGA(const char *szFileName, 
    int32_t *iWidth, 
    int32_t *iHeight, 
    int32_t *iComponents, 
    uint32_t *eFormat,
    std::vector<int8_t> &pixels)
{
  FILE *pFile;			// File pointer
  TGAHEADER tgaHeader;		// TGA file header
  unsigned long lImageSize;		// Size in bytes of image
  short sDepth;			// Pixel depth;

  // Default/Failed values
  *iWidth = 0;
  *iHeight = 0;
  *eFormat = GL_BGR_EXT;
  *iComponents = GL_RGB8;

  // Attempt to open the fil
  pFile = fopen(szFileName, "rb");
  if(pFile == NULL)
    return false;

  // Read in header (binary)
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
  sDepth = tgaHeader.bits / 8;

  // Put some validity checks here. Very simply, I only understand
  // or care about 8, 24, or 32 bit targa's.
  if(tgaHeader.bits != 8 && tgaHeader.bits != 24 && tgaHeader.bits != 32)
    return NULL;

  // Calculate size of image buffer
  lImageSize = tgaHeader.width * tgaHeader.height * sDepth;
  pixels.resize(lImageSize);

  // Read in the bits
  // Check for read error. This should catch RLE or other 
  // weird formats that I don't want to recognize
  if(fread(&pixels[0], lImageSize, 1, pFile) != 1)
  {
    pixels.clear();
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


  // Done with File
  fclose(pFile);

  return true;
}

