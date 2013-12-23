#include "Screenshot.h"
#include "Tga.h"
#include "../shared/glforward.h"
#include <iostream>

bool SaveScreen(const std::string &fileName)
{
  return SaveScreen(fileName, GL_FRONT);
}

bool SaveScreen(const std::string &fileName, uint32_t readBuffer)
{
// Save to TGA
  GLint viewport[4];
  glGetIntegerv(GL_VIEWPORT, viewport);
  uint32_t imageSize = viewport[2] * 3 * viewport[3];

  std::vector<uint8_t> pixels(imageSize);
  glPixelStorei(GL_PACK_ALIGNMENT, 1);
  glPixelStorei(GL_PACK_ROW_LENGTH, 0);
  glPixelStorei(GL_PACK_SKIP_ROWS, 0);
  glPixelStorei(GL_PACK_SKIP_PIXELS, 0);

  GLint oldBuffer;
  glGetIntegerv(GL_READ_BUFFER, &oldBuffer);
  glReadBuffer(readBuffer);
  glReadPixels(0, 0, viewport[2], viewport[3], GL_BGR_EXT, GL_UNSIGNED_BYTE, 
      &pixels[0]);
  glReadBuffer(oldBuffer);

  time_t rawTime;
  time(&rawTime);
  tm *localTime = localtime(&rawTime);
  char buffer[80];
  strftime(buffer, 80, "%y%m%d%H%M%S", localTime);
  std::string stampedFileName = fileName + buffer + ".tga";
  std::cout << "\nWriting " << stampedFileName << std::endl;
  return gltWriteTGA(stampedFileName.c_str(), 24, viewport[2], viewport[3], &pixels[0]);
}

bool SaveTexture2Imp(const std::string &fileName, uint32_t target)
{
  int32_t width = 0, height = 0;
  glGetTexLevelParameteriv(target, 0, GL_TEXTURE_WIDTH, &width);
  glGetTexLevelParameteriv(target, 0, GL_TEXTURE_HEIGHT, &height);
  uint32_t imageSize = width * height * 3;

  glPixelStorei(GL_PACK_ALIGNMENT, 1);
  glPixelStorei(GL_PACK_ROW_LENGTH, 0);
  glPixelStorei(GL_PACK_SKIP_ROWS, 0);
  glPixelStorei(GL_PACK_SKIP_PIXELS, 0);
  std::vector<uint8_t> pixels(imageSize);
  glGetTexImage(target, 0, GL_BGR_EXT, GL_UNSIGNED_BYTE, &pixels[0]);

  time_t rawTime;
  time(&rawTime);
  tm *localTime = localtime(&rawTime);
  char buffer[80];
  strftime(buffer, 80, "_%y%m%d%H%M%S", localTime);
  std::string stampedFileName = fileName + buffer + ".tga";
  std::cout << "\nSaving texture " << stampedFileName << std::endl;
  return gltWriteTGA(stampedFileName.c_str(), 24, width, height, &pixels[0]);
}

bool SaveTextureRect(const std::string& fileName)
{
  return SaveTexture2Imp(fileName, GL_TEXTURE_RECTANGLE);
}

bool SaveTexture2D(const std::string& fileName)
{
  return SaveTexture2Imp(fileName, GL_TEXTURE_2D);
}
