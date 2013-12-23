/*
 *  \file textures.cpp
 *  \brief textures.cpp
 *  \author Daniel Grigg 
 *  \date 10/10/10.
 *  textures
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 */

#include "glforward.h"
#ifdef __APPLE__
#include <glut/glut.h>
#else
#define FREEGLUT_STATIC
#include <GL/glut.h>
#endif
#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <tr1/memory>
#include <lexi/Color/LColor.hpp>
#include <lexi/Geometry/LPoint.hpp>
#include <lexi/Geometry/LTransform.hpp>
#include <lexi/IO/LTextFileReader.hpp>
#include <lexi/GL/LShader.hpp>
#include "../shared/Screenshot.h"
#include "../shared/Tga.h"
#include "../shared/GeometryBatch.h"
#include <lexi/Util/LStopwatch.hpp>
#include "../shared/ShapeFactory.h"
#include "../shared/Textures.h"
#include "../shared/GLUtility.h"
#include <utility>


#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/fstream.hpp>    // ditto
#include <boost/filesystem/convenience.hpp>

using namespace boost::filesystem;
using namespace std;

enum
{
  kPosOffset,
  kColorOffset
};

const std::string mediaPath = "../Resources";

enum 
{
  kModelView,
  kMVP,
  kMVPLogo,
  kTime,
  kColorMap,
  kModulationMap,
  kModulationBlend,
  kColorMapLogo,
  kUniformMax
};

GLuint uniforms[kUniformMax];
bool gReady;
LTransform g_projection;
LTransform g_screenProjection;
LShader g_shader;
std::vector<GLuint> g_textures;
GLuint g_textureLogo;
int g_selectedTexture = 0;
static bool g_paused = false;
static LStopwatch g_appTimer;
LShader g_shaderRect;
GeometryBatch g_batchLogo;

std::vector<GeometryBatch> g_batches;
int g_selectedBatch = 0;

bool filesInDir(const path & dirPath, std::vector<path> &filePaths)
{
  if (!exists(dirPath)) return false;

  directory_iterator endIter; // default construction yields past-the-end
  for (directory_iterator iter( dirPath ); iter != endIter; ++iter )
  {
    filePaths.push_back(*iter);
  }
  return true;
}

void loadTexture(path &p, GLuint wrapMode, GLuint minFilter, GLuint magFilter)
{
  if (extension(p) == ".tga" || extension(p) == ".TGA")
  {
    g_textures.push_back(loadTextureFromFile(p.string(), wrapMode, minFilter, magFilter));
    cout << "Loaded texture (" << g_textures.size()-1 << ", " << p << ")\n";
  }
}

bool loadTextures(GLuint wrapMode, GLuint minFilter, GLuint magFilter)
{
  vector<path> resources;
  filesInDir("../Resources", resources);
  for_each(resources.begin(), resources.end(), 
      boost::bind(loadTexture, _1, wrapMode, minFilter, magFilter));

  g_textureLogo = loadTextureRectFromFile("../Resources/OpenGL-Logo.tga", 
      GL_CLAMP_TO_EDGE, GL_NEAREST, GL_NEAREST);
  if (0 == g_textureLogo) return false;
  cout << "Loaded texture rect (" << g_textureLogo << ", " << "OpenGL-Logo.tga" << ")\n";


  if (checkGLErrors()) cout << "loaded textures" << endl;
  return true;
}

bool loadShaderPair(LShader &shader, const std::string &name)
{
  const std::string fp = name + ".fp";
  const std::string vp = name + ".vp";
  vector<string> sources;
  sources.push_back(fp);
  sources.push_back(vp);

  vector<std::pair<string,int> > attributes;
  attributes.push_back(std::make_pair("vVertexPos", (int)GeometryBatch::kPosOffset));
//  attributes.push_back(std::make_pair("vNormal", (int)GeometryBatch::kNormalOffset));
  attributes.push_back(std::make_pair("vTexCoord", (int)GeometryBatch::kUV0Offset));

  shader.setSources(sources, attributes);
  bool result = shader.build();
  cout << "Shader: " << shader << endl;
  if (checkGLErrors()) cout << "Initialised shaders" << endl;
  return result;
}

bool initShaders()
{
  bool result = true;
  result = result && loadShaderPair(g_shader, "textures"); 
  result = result && loadShaderPair(g_shaderRect, "texture_rect"); 

  loadTextures(GL_REPEAT, GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR);

  if (checkGLErrors()) cout << "Initialised shaders" << endl;
  return result;
}

void initGeometry()
{
  if (!checkGLErrors()) return;
  int n = 0;
  g_batches.resize(4);
  makePyramid(g_batches[n++], false, true);
  makeRing(g_batches[n++], 2.0f, 2.0f, 32, false, true);
  //  makeFan(g_batches[n++], 2.0f, 2.0f, 32, false, true);
  makeSphere(g_batches[n++], 2.2f, 32, true, true);
  makeQuad(g_batches[n++], 2.0f, 2.0f, false, true);

  makeRect(g_batchLogo, 300, 155, false, true);
  if (checkGLErrors()) cout << "Initialised geometry" << endl;
}

void drawLogo()
{
  glUseProgram(g_shaderRect.program());
  LTransform MVP = g_screenProjection;
  glUniformMatrix4fv(uniforms[kMVPLogo], 1, GL_FALSE, MVP.array());
  glUniform1i(uniforms[kColorMapLogo], 0);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glDisable(GL_DEPTH_TEST);

  glBindTexture(GL_TEXTURE_RECTANGLE, g_textureLogo);
  g_batchLogo.bind();
  g_batchLogo.draw();

  glEnable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  if (!checkGLErrors()) cout << "drawLogo error" << endl;
}

void drawGeometry()
{
  float t = g_appTimer.timeElapsed_s();
  glUseProgram(g_shader.program());
  //  glUniform1f(uniforms[kTime], t);
  glUniform1i(uniforms[kColorMap], 0);
  glUniform1i(uniforms[kModulationMap], 1);
//  glUniform1i(uniforms[kModulationBlend], 1.0);
  LTransform M = LTranslate(.0f, 0.5, -21.0 + 12.0f * sinf(.17 * t)) * 
    LRotateX(LRadians(30)) * LRotateY(LRadians(7.0f * t));
  LTransform MVP = g_projection * M;
  //  glUniformMatrix4fv(uniforms[kModelView], 1, GL_FALSE, M.asMatrix().asArray());
  glUniformMatrix4fv(uniforms[kMVP], 1, GL_FALSE, MVP.array());

  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, g_textures[g_selectedTexture]);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, g_textures[(g_selectedTexture + 1) % g_textures.size()]);
  glActiveTexture(GL_TEXTURE0);
  g_batches[g_selectedBatch].bind();
  g_batches[g_selectedBatch].draw();
  if (!checkGLErrors()) cout << "drawGeometry error" << endl;
}

void setupRC()
{
  glClearColor(0.15f, 0.15f, 0.15f,1.0f);
  if (!initShaders() )
  {
    cout << "init shaders error\n";
    return;
  }

  for (int i = 0; i < kUniformMax; ++i) uniforms[i] = 0;
  //  uniforms[kModelView] = glGetUniformLocation(g_shader.program(), "mvMatrix");
  uniforms[kMVP] = glGetUniformLocation(g_shader.program(), "mvpMatrix");
  //  uniforms[kTime] = glGetUniformLocation(g_shader.program(), "uTime");
  uniforms[kColorMap] = glGetUniformLocation(g_shader.program(), "colorMap");
  uniforms[kModulationMap] = glGetUniformLocation(g_shader.program(), "modulationMap");

  uniforms[kColorMapLogo] = glGetUniformLocation(g_shaderRect.program(), "colorMap");
  uniforms[kMVPLogo] = glGetUniformLocation(g_shaderRect.program(), "mvpMatrix");

  for (int i = 0; i < kUniformMax; ++i)
  {
    cout << "uniform[" << i << "]: " << uniforms[i] << endl;
  }
  initGeometry();
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);

  gReady = true;
  if (checkGLErrors()) cout << "Setup RC" << endl;
}

void shutdownRC(void)
{
  g_shader.clean();
  if (checkGLErrors()) cout << "Shutdown RC" << endl;
}

void RenderScene(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  if (gReady)
  {
    drawGeometry();
    drawLogo();
  }
  glutSwapBuffers();
  glutPostRedisplay();
}

void ChangeSize(int w, int h)
{
  GLfloat fAspect;

  if(h == 0) h = 1;

  // Set Viewport to window dimensions
  glViewport(0, 0, w, h);

  fAspect = (GLfloat)w/(GLfloat)h;
  g_projection = LPerspective(LRadians(40.0f), fAspect, 1.0f, 100.0f);
  g_screenProjection = LOrtho(0, w, 0, h);
}

static int selectedMinFilter = 5;
static int selectedMagFilter = 1;

void KeyPressFunc(unsigned char key, int x, int y)
{
  if(key == 'n' || key == 'N' || key == 0x20)
  {
    g_selectedBatch = (g_selectedBatch + 1) % g_batches.size();
    cout << "Switching to " << g_batches[g_selectedBatch].name << " batch.\n";
  }
  else if (key == 'a' || key == 'A')
  {
    static float anisotropy = 0.0f;
    float largest;
    glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, &largest);   
    anisotropy += 1.0f;
    anisotropy = (anisotropy > largest) ? 1.0f : anisotropy;
    for (int i = 0; i < g_textures.size(); ++i)
    {
      glBindTexture(GL_TEXTURE_2D, g_textures[i]);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, anisotropy);
    }

    cout << "texture anistropy " << anisotropy <<
      "(" << largest << " max)" << endl;
  }
  else if(key == 't' || key == 'T')
  {
    g_selectedTexture = (g_selectedTexture + 1) % g_textures.size();
  }
  else if(key == 'p' || key == 'P')
  {
    g_paused = !g_paused;
    g_paused ? g_appTimer.pause() : g_appTimer.play();
    cout << (g_paused ? "Pausing" : "Un-pausing") << endl;
  }
  else if (key == 'b' || key == 'B')
  {
    g_shader.build();
  }
  else if (key == 'm' || key == 'm' || key == 'k' || key == 'K')
  {
    uint32_t minFilters[] = {GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
      GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_NEAREST, 
      GL_LINEAR_MIPMAP_LINEAR};
    uint32_t magFilters[] = {GL_NEAREST, GL_LINEAR};
    string magFilterNames[] = {"nearest", "linear"};
    string minFilterNames[] = {"nearest", "linear", "nearest_mipmap_nearest",
      "nearest_mipmap_linear", "linear_mipmap_nearest",
      "linear_mipmap_linear"};
    selectedMinFilter = (key == 'm' || key == 'M') ? 
      (selectedMinFilter + 1) % 6 : selectedMinFilter;
    selectedMagFilter = (key == 'k' || key == 'K') ?
      (selectedMagFilter + 1) % 2 : selectedMagFilter;
    cout << "Min/Mag filter set to " << minFilterNames[selectedMinFilter]
      << "/" << magFilterNames[selectedMagFilter] << endl;

    for (int i = 0; i < g_textures.size(); ++i)
    {
      glDeleteTextures(1, &g_textures[i]);
    }
    loadTextures(GL_REPEAT,
        minFilters[selectedMinFilter],
        magFilters[selectedMagFilter]);
  }
  else if (key == 'd' || key == 'D')
  {
    static bool depthTest = true;
    depthTest = !depthTest;
    depthTest ? glEnable(GL_DEPTH_TEST) : glDisable(GL_DEPTH_TEST);
    cout << "Depth Test " << (depthTest ? "enabled" : "disabled") << "\n";
  }
  else if (key == '\\')
  {
    glutSwapBuffers();
    SaveScreen("screenshot");
  }
  glutPostRedisplay();
}

int main(int argc, char *argv[])
{
  gReady = false;
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowSize(1280, 720);
  glutCreateWindow("textures");
  glutReshapeFunc(ChangeSize);
  glutDisplayFunc(RenderScene);
  glutKeyboardFunc(KeyPressFunc);

  GLenum err = glewInit();
  if (GLEW_OK != err) 
  {
    fprintf(stderr, "GLEW Error: %s\n", glewGetErrorString(err));
    return 1;
  }
  setupRC();
  g_appTimer.reset();
  if (gReady) glutMainLoop();
  shutdownRC();
  return 0;
}




