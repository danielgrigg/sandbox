
#include "Textures.h"
#include <iostream>
#include <vector>
#include "Tga.h"
#include <boost/format.hpp>

GLuint loadTextureRectFromFile(const std::string &fileName, GLenum wrapMode,
    GLenum minFilter, GLenum magFilter)
{
  std::vector<uint8_t> pixels;
  uint32_t width; uint32_t height; uint32_t components; uint32_t format;
  if (!gltLoadTGA(fileName.c_str(), &width, &height, &components, &format, pixels))
  {
    std::cerr << fileName << " load failed.\n";
    return 0;
  }

  GLuint texture;
  glGenTextures(1, &texture);  
  glBindTexture(GL_TEXTURE_RECTANGLE, texture);

  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_WRAP_S, wrapMode);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_WRAP_T, wrapMode);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MIN_FILTER, minFilter);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MAG_FILTER, magFilter);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  //GL_RGB, GL_COMPRESSED_RGB
  glTexImage2D(GL_TEXTURE_RECTANGLE, 0, components, width, height, 0, 
      format, GL_UNSIGNED_BYTE, &pixels[0]);
  return texture;
}


GLuint loadTextureFromFile(const std::string &fileName, GLenum wrapMode,
    GLenum minFilter, GLenum magFilter, bool anisotropic)
{
  std::vector<uint8_t> pixels;
  uint32_t width; uint32_t height; uint32_t components; uint32_t format;
  if (!gltLoadTGA(fileName.c_str(), &width, &height, &components, &format, pixels))
  {
    std::cerr << fileName << " load failed.\n";
    return 0;
  }

  GLuint texture;
  glGenTextures(1, &texture);  
  glBindTexture(GL_TEXTURE_2D, texture);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wrapMode);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrapMode);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, minFilter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, magFilter);
  if (anisotropic)
  {
    float largest;
    glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, &largest);   
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, largest);
  }

  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glTexImage2D(GL_TEXTURE_2D, 0, components, width, height, 0, 
      format, GL_UNSIGNED_BYTE, &pixels[0]);
  if (minFilter == GL_LINEAR_MIPMAP_LINEAR ||
      minFilter == GL_LINEAR_MIPMAP_NEAREST ||
      minFilter == GL_NEAREST_MIPMAP_LINEAR ||
      minFilter == GL_NEAREST_MIPMAP_NEAREST)
  {
    glGenerateMipmap(GL_TEXTURE_2D);
  }
  return texture;
}


void loadTextureArraySliceFromFile(const std::string &fileName, int slice)
{
  std::vector<uint8_t> pixels;
  uint32_t width; uint32_t height; uint32_t components; uint32_t format;
  
  const std::string sliceFileName = 
    (boost::format("%1$s%2$02d.tga") % fileName % slice).str();
  std::cout << "Loading slice " << sliceFileName << std::endl;

  if (!gltLoadTGA(sliceFileName.c_str(), &width, &height, &components, &format, pixels))
  {
    std::cerr << fileName << " load failed.\n";
    return;
  }
  glTexSubImage3D(GL_TEXTURE_2D_ARRAY, 0, 0, 0, slice, 
      width, height, 1, format, GL_UNSIGNED_BYTE, &pixels[0]);
}

GLuint loadTextureArrayFromFiles(const std::string &fileName, 
    int width, int height, int count, GLenum wrapMode,
    GLenum minFilter, GLenum magFilter, bool anisotropic)
{
  GLuint texture;
  glGenTextures(1, &texture);  
  glBindTexture(GL_TEXTURE_2D_ARRAY, texture);

  glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_S, wrapMode);
  glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_T, wrapMode);
  glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER, minFilter);
  glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER, magFilter);

  if (anisotropic)
  {
    float largest;
    glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, &largest);   
    glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAX_ANISOTROPY_EXT, largest);
  }

  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glTexImage3D(GL_TEXTURE_2D_ARRAY, 0, GL_RGBA, width, height, count, 0, 
      GL_BGRA, GL_UNSIGNED_BYTE, NULL);

  for (int i = 0; i < count; ++i)
  {
    loadTextureArraySliceFromFile(fileName, i);
  }
  if (minFilter == GL_LINEAR_MIPMAP_LINEAR ||
      minFilter == GL_LINEAR_MIPMAP_NEAREST ||
      minFilter == GL_NEAREST_MIPMAP_LINEAR ||
      minFilter == GL_NEAREST_MIPMAP_NEAREST)
  {
    glGenerateMipmap(GL_TEXTURE_2D_ARRAY);
  }
  return texture;

}

bool loadCubeMapFaceFromFile(const std::string &fileName, GLuint face)
{
  std::vector<uint8_t> pixels;
  uint32_t width; uint32_t height; uint32_t components; uint32_t format;
  if (!gltLoadTGA(fileName.c_str(), &width, &height, &components, &format, pixels))
  {
    std::cerr << fileName << " load failed.\n";
    return false;
  }

  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glTexImage2D(face, 0, components, width, height, 0, 
      format, GL_UNSIGNED_BYTE, &pixels[0]);
  if (glGetError() != GL_NO_ERROR) return false;
  return true;
}

GLuint loadCubeMapFromFaceFiles(const std::string &fileName)
{
  const char * faceFileNames[6] = {
    "_pos_x.tga", "_neg_x.tga", "_pos_y.tga", 
    "_neg_y.tga", "_pos_z.tga", "_neg_z.tga" };

  const GLuint faceGLNames[6] = {
    GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
    GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
    GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z };
  
  GLuint texture;
  glGenTextures(1, &texture);  
  glBindTexture(GL_TEXTURE_CUBE_MAP, texture);

  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

  for (int i = 0; i < 6; ++i)
  {
    const std::string face = fileName + faceFileNames[i]; 
    if (!loadCubeMapFaceFromFile(face, faceGLNames[i]))
    {
      std::cerr << "Error loading " << face << std::endl;
    }
  }

  glGenerateMipmap(GL_TEXTURE_CUBE_MAP);
  return texture;
}
