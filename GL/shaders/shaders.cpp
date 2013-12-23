/*
 *  \file shaders.cpp
 *  \brief shaders.cpp
 *  \author Daniel Grigg 
 *  \date 10/10/10.
 *  shaders
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 */

#include "../shared/glforward.h"
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
#include <lexi/Util/LStopwatch.hpp>
#include "../shared/Screenshot.h"
#include "../shared/Tga.h"
#include "../shared/ShapeFactory.h"
#include "../shared/Textures.h"
#include "../shared/GLUtility.h"

using namespace std;

const std::string mediaPath = "../Resources";

enum 
{
  kMV,
  kNormalMatrix,
  kMVP,
  kTime,
  kDissolve,
  kLightPosition,
  kDiffuseMap,
  kOutlineTime,
  kOutlineMV,
  kOutlineMVP,
  kUniformMax
};

GLuint uniforms[kUniformMax];
bool gReady = false;
LTransform g_projection;
std::vector<LShader> g_shaders;
int g_selectedShader = 0;
static bool g_paused = false;
static LStopwatch g_appTimer;
static bool g_drawOutline = true;
std::vector<GeometryBatch> g_batches;
int g_selectedBatch = 0;
GLuint g_texture;
GLuint g_toonTexture;
int g_toonId = 0;
static LShader g_outlineShader;

bool loadShaderPair(LShader &shader, const std::string &name)
{
  const std::string fp = name + ".fp";
  const std::string vp = name + ".vp";
  vector<string> sources;
  sources.push_back(fp);
  sources.push_back(vp);

  vector<std::pair<string,int> > attributes;
  attributes.push_back(std::make_pair("vVertexPos", (int)GeometryBatch::kPosOffset));
  attributes.push_back(std::make_pair("vNormal", (int)GeometryBatch::kNormalOffset));
  attributes.push_back(std::make_pair("vTexCoord", (int)GeometryBatch::kUV0Offset));
  //attributes.push_back(std::make_pair("vColor", (int)GeometryBatch::kUV0Offset));

  shader.setSources(sources, attributes);
  bool result = shader.build();
  cout << "Shader: " << shader << endl;
  return result;
}

bool initShaders()
{
  g_shaders.resize(6);
  int n = 0;
  bool result = true;
  result = result && loadShaderPair(g_shaders[n++], "dissolve"); 
  result = result && loadShaderPair(g_shaders[n++], "diffuse_point_texture"); 
  result = result && loadShaderPair(g_shaders[n++], "diffuse_point"); 
  result = result && loadShaderPair(g_shaders[n++], "color"); 
  result = result && loadShaderPair(g_shaders[n++], "color_wave"); 
  g_toonId = n;
  result = result && loadShaderPair(g_shaders[n++], "toon"); 
  result = result && loadShaderPair(g_outlineShader, "outline");

  g_texture = loadTextureFromFile("../Resources/Clouds.tga", GL_REPEAT, 
      GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR);
  cout << "texture: " << g_texture << endl;
  float largest;
  glBindTexture(GL_TEXTURE_2D, g_texture);
  glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, &largest);   
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, largest);

  glGenTextures(1, &g_toonTexture);
  glBindTexture(GL_TEXTURE_1D, g_toonTexture);
  GLubyte textureData[] = { 32, 32,  0,
                            64, 64,  0,
                            128, 128,  0,
                            255, 255,  0};
  glTexImage1D(GL_TEXTURE_1D, 0, GL_RGB, 4, 0, GL_RGB, GL_UNSIGNED_BYTE, &textureData);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);

  if (checkGLErrors()) cout << "Initialised shaders" << endl;
  return result;
}

void initGeometry()
{
  if (!checkGLErrors()) return;
  int n = 0;
  LColor c(1.0f, 0.0f, 0.0f, 1.0f);
  g_batches.resize(3);
//  makePyramid(g_batches[n++], false, true, &c);
  makeCube(g_batches[n++], 2.0f, true, true);
  makeRing(g_batches[n++], 3.0f, 1.0f, 32, true, true, NULL);
  makeSphere(g_batches[n++], 2.2f, 32, true, true);
//  makeFan(g_batches[n++], 2.0f, 1.5f, 32, false, true, &c);

  if (checkGLErrors()) cout << "Initialised geometry" << endl;
}

void drawGeometryOutline(float t, const LTransform &M, const LTransform &MVP)
{
  glUseProgram(g_outlineShader.program());
  glUniform1f(uniforms[kOutlineTime], t);
  glUniformMatrix4fv(uniforms[kOutlineMV], 1, GL_FALSE, M.array());
  glUniformMatrix4fv(uniforms[kOutlineMVP], 1, GL_FALSE, MVP.array());

  glPolygonOffset(-1, -1);
  glEnable(GL_POLYGON_OFFSET_LINE);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glLineWidth(2.5f);

//  g_outlineBatches[g_selectedBatch].bind();
  g_batches[g_selectedBatch].draw();
  
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glDisable(GL_POLYGON_OFFSET_LINE);
  glLineWidth(1.0f);
  glDisable(GL_BLEND);
  glDisable(GL_LINE_SMOOTH);
}

void drawGeometry()
{
  float t = g_appTimer.timeElapsed_s();
  glUseProgram(g_shaders[g_selectedShader].program());
  LTransform M = LTranslate(.0f, 0.5, -9.0) *
    LRotateX(LRadians(15.0f)) * LRotateX(LRadians(-7.0f * t));
  LTransform MVP = g_projection * M;
  LTransform normalTransform(M.inverse().matrix().transpose());

  float dissolve = 0.5f + 0.5f  * sinf(t);
  glUniform1f(uniforms[kDissolve], dissolve);
  glUniform1f(uniforms[kTime], t);
  glUniformMatrix4fv(uniforms[kMV], 1, GL_FALSE, M.array());
  glUniformMatrix4fv(uniforms[kNormalMatrix], 1, GL_FALSE, normalTransform.array());
  glUniformMatrix4fv(uniforms[kMVP], 1, GL_FALSE, MVP.array());

  LPoint lp(-5,5,5);
  glUniform3fv(uniforms[kLightPosition], 1, (float*)&lp);
  glUniform1i(uniforms[kDiffuseMap], 0);

  g_batches[g_selectedBatch].bind();
  g_batches[g_selectedBatch].draw();

  if (g_drawOutline) drawGeometryOutline(t, M, MVP);
  checkGLErrors();
}

void setupUniforms()
{
  const LShader &selected = g_shaders[g_selectedShader];
  uniforms[kMV] = glGetUniformLocation(selected.program(), "mvMatrix");
  uniforms[kNormalMatrix] = 
    glGetUniformLocation(selected.program(), "normalMatrix");
  uniforms[kMVP] = glGetUniformLocation(selected.program(), "mvpMatrix");
  uniforms[kDissolve] = glGetUniformLocation(selected.program(), "dissolve");
  uniforms[kTime] = glGetUniformLocation(selected.program(), "uTime");
  uniforms[kLightPosition] = glGetUniformLocation(selected.program(), 
      "uLightPosition");
  uniforms[kDiffuseMap] = glGetUniformLocation(selected.program(), 
      "diffuseMap");
  uniforms[kOutlineMV] = glGetUniformLocation(g_outlineShader.program(), "mvMatrix");
  uniforms[kOutlineMVP] = glGetUniformLocation(g_outlineShader.program(), "mvpMatrix");
  uniforms[kOutlineTime] = glGetUniformLocation(g_outlineShader.program(), "uTime");

  glActiveTexture(GL_TEXTURE0);
  if (g_toonId == g_selectedShader)
  {
    glBindTexture(GL_TEXTURE_1D, g_toonTexture);
  }
  else
  {
    glBindTexture(GL_TEXTURE_2D, g_texture);
  }

  checkGLErrors();
}

void setupRC()
{
  glClearColor(0.15f, 0.15f, 0.15f,1.0f);
  if (!initShaders() )
  {
    cout << "init shaders error\n";
    gReady = false;
    return;
  }
  else
  {
    setupUniforms();
  }
  initGeometry();

  glEnable(GL_DEPTH_TEST);

  gReady = true;
  if (checkGLErrors()) cout << "Setup RC" << endl;
}

void shutdownRC(void)
{
  for_each(g_shaders.begin(), g_shaders.end(), 
      boost::bind(&LShader::clean, _1));
  if (checkGLErrors()) cout << "Shutdown RC" << endl;
}

void RenderScene(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  if (gReady)
  {
    glEnable(GL_DITHER);
    drawGeometry();
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
}

void KeyPressFunc(unsigned char key, int x, int y)
{
  if(key == 'n' || key == 'N' || key == 0x20)
  {
    g_selectedBatch = (g_selectedBatch + 1) % g_batches.size();
    cout << "Switching to " << g_batches[g_selectedBatch].name << " batch.\n";
  }
  else if (key == 'b' || key == 'B')
  {
    g_shaders[g_selectedShader].build();
    setupUniforms();
  }
  else if (key == 'o' || key == 'O')
  {
    g_drawOutline = !g_drawOutline;
    cout << (g_drawOutline ? "drawing" : "not drawing") << " outline.\n";
  }
  else if (key == 's' || key == 'S')
  {
    g_selectedShader = (g_selectedShader + 1) % g_shaders.size();
    setupUniforms();
    cout << "Switching shader: " << g_shaders[g_selectedShader].toString() << "\n";
  }
  else if(key == 'p' || key == 'P')
  {
    g_paused = !g_paused;
    g_paused ? g_appTimer.pause() : g_appTimer.play();
    cout << (g_paused ? "Pausing" : "Un-pausing") << endl;
  }
  else if (key == '\\')
  {
    glutSwapBuffers();
    SaveScreen("shaders");
  }
  glutPostRedisplay();
}

int main(int argc, char *argv[])
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowSize(1280, 720);
  glutCreateWindow("shaders");
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
  if (gReady) glutMainLoop();
  shutdownRC();
  return 0;
}




