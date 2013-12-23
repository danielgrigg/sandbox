#ifndef TEXTURES_H
#define TEXTURES_H

#include "glforward.h"
#include <string>

GLuint loadTextureRectFromFile(const std::string &fileName, GLenum wrapMode,
    GLenum minFilter, GLenum magFilter);
GLuint loadTextureFromFile(const std::string &fileName, GLenum wrapMode,
    GLenum minFilter, GLenum magFilter, bool anistropic = false);

// load a cubemap from files of the form (a_pos_x, a_neg_x, ..., a_neg_z)
GLuint loadCubeMapFromFaceFiles(const std::string &fileName);

GLuint loadTextureArrayFromFiles(const std::string &fileName, 
    int width, int height, int count, GLenum wrapMode,
    GLenum minFilter, GLenum magFilter, bool anisotropic = false);

#endif
