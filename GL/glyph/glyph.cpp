/*
 *  \file glyph.cpp
 *  \brief glyph.cpp
 *  \author Daniel Grigg 
 *  \date 10/10/10.
 *  glyph
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
  kModelView,
  kMVP,
  kMVGlyph,
  kMVPGlyph,
  kTime,
  kUniformMax,
  kGlyphMap,
};

GLuint uniforms[kUniformMax];
bool gReady = false;
LTransform g_projection;
LTransform g_screenProjection;
LShader g_shader;
static bool g_paused = false;
static LStopwatch g_appTimer;
GLuint gTextureGlyph;
GeometryBatch gBatchGlyph;
static uint32_t gViewWidth = 0;
static uint32_t gViewHeight = 0;


std::vector<GeometryBatch> g_batches;
int g_selectedBatch = 0;

GLuint loadGlyphTexture()
{
  GLuint texture;

  texture = loadTextureRectFromFile("Q.tga", 
      GL_CLAMP_TO_EDGE, GL_NEAREST, GL_NEAREST);
  return texture;
}

bool initShaders()
{
  vector<string> sources;
  sources.push_back("glyph.fp");
  sources.push_back("glyph.vp");

  vector<std::pair<string,int> > attributes;
  attributes.push_back(std::make_pair("vVertexPos", (int)GeometryBatch::kPosOffset));
  //attributes.push_back(std::make_pair("vColor", (int)GeometryBatch::kColorOffset));
  attributes.push_back(std::make_pair("vTexCoord", (int)GeometryBatch::kUV0Offset));
  g_shader.setSources(sources, attributes);
  bool result = g_shader.build();
  cout << "Shader: " << g_shader << endl;

//  gTextureLogo = loadTextureRectFromFile("../Resources/OpenGL-Logo.tga", 
//      GL_CLAMP_TO_EDGE, GL_NEAREST, GL_NEAREST);

  gTextureGlyph = loadGlyphTexture();

  if (checkGLErrors()) cout << "Initialised shaders" << endl;
  return result;
}

void initGeometry()
{
  if (!checkGLErrors()) return;
  int n = 0;
  LColor c(1.0f, 0.0f, 0.0f, 1.0f);
  g_batches.resize(1);
  makeRect(gBatchGlyph, 157, 248, false, true);
  if (checkGLErrors()) cout << "Initialised geometry" << endl;
}

void drawGlyph()
{
  glUseProgram(g_shader.program());
  LTransform MV = LTranslate(gViewWidth - gViewWidth / 2 - (157/2), 
                             gViewHeight - gViewHeight / 2 - (248/2), 0);
  LTransform MVP = g_screenProjection * MV;
  glUniformMatrix4fv(uniforms[kMVPGlyph], 1, GL_FALSE, MVP.array());
  glUniform1i(uniforms[kGlyphMap], 0);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glDisable(GL_DEPTH_TEST);

  glBindTexture(GL_TEXTURE_RECTANGLE, gTextureGlyph);
  gBatchGlyph.bind();
  gBatchGlyph.draw();

  glEnable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  if (!checkGLErrors()) cout << "drawGlyph error" << endl;

}

void setupRC()
{
  glClearColor(0.15f, 0.15f, 0.15f,1.0f);
  if (!initShaders() )
  {
    cout << "init shaders error\n";
    return;
  }

//  uniforms[kMVGlyph] = glGetUniformLocation(g_shader.program(), "mvMatrix");
  uniforms[kMVPGlyph] = glGetUniformLocation(g_shader.program(), "mvpMatrix");
  uniforms[kTime] = glGetUniformLocation(g_shader.program(), "uTime");
  uniforms[kGlyphMap] = glGetUniformLocation(g_shader.program(), "glyphMap");
  initGeometry();

  glEnable(GL_DEPTH_TEST);

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
    drawGlyph();
  }
  glutSwapBuffers();
  glutPostRedisplay();
}

void ChangeSize(int w, int h)
{
  gViewWidth = w;
  gViewHeight = h;
  GLfloat fAspect;

  if(h == 0) h = 1;

  // Set Viewport to window dimensions
  glViewport(0, 0, w, h);

  fAspect = (GLfloat)w/(GLfloat)h;
  g_projection = LPerspective(LRadians(40.0f), fAspect, 1.0f, 100.0f);

  g_screenProjection = LOrtho(0, w, 0, h);
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
    g_shader.build();
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
    SaveScreen("glyph");
  }
  glutPostRedisplay();
}

int main(int argc, char *argv[])
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowSize(1280, 720);
  glutCreateWindow("glyph");
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




