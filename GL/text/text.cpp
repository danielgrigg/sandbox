/*
 *  \file text.cpp
 *  \brief text.cpp
 *  \author Daniel Grigg 
 *  \date 10/10/10.
 *  text
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

#include "../shared/LFont.hpp"

using namespace std;

const std::string mediaPath = "../Resources";

enum 
{
  kModelView,
  kMVP,
  kTime,
  kTextMV,
  kPTextMVP,
  kUniformMax
};
GLuint uniforms[kUniformMax];
bool gReady = false;
LTransform g_projection;
//LShader g_shader;
static bool g_paused = false;
static LStopwatch g_appTimer;
GeometryBatch gBatchGlyph;
static uint32_t gViewWidth = 0;
static uint32_t gViewHeight = 0;
static bool gShowFps = true;

std::vector<GeometryBatch> g_batches;
int g_selectedBatch = 0;

typedef std::tr1::shared_ptr<LFont> LFontPtr;
LFontPtr g_font;
LFontPtr gSystemFont;

bool initContent()
{
  bool result = true;
  const int pts = 128; const int dpi = 72;
  //g_font = LFontPtr(new LFont("Consolas Bold", pts, dpi, "/Library/Fonts/Microsoft/"));
  g_font = LFontPtr(new LFont("Pilgiche", pts, dpi, "/Library/Fonts/"));
  gSystemFont = LFontPtr(new LFont("Consolas", 20, dpi, "/Library/Fonts/Microsoft/"));
  if (!checkGLErrors()) return false;
  return result;
}

bool initGeometry()
{
  if (!checkGLErrors()) return false;
  int n = 0;
  LColor c(1.0f, 0.0f, 0.0f, 1.0f);
  g_batches.resize(1);
  makeQuad(g_batches[n++], 2.0, 2.0, false, true);
  if (checkGLErrors()) cout << "Initialised geometry" << endl;
  return true;
}

void drawGeometry(float appTime)
{
  return;
  for (uint32_t i = 0; i < 100; ++i)
  {
    const std::string str = boost::lexical_cast<string>(random() % 1000);
    g_font->write(random() % gViewWidth , random() % gViewHeight, str);
  }
  checkGLErrors();
}

void setupRC()
{
  glClearColor(0.15f, 0.15f, 0.15f,1.0f);
  if (!initContent() )
  {
    cout << "init content error\n";
    return;
  }
  
  if (!initGeometry())
  {
    cout << "init geometry error\n";
    return;
  }
  
  glEnable(GL_DEPTH_TEST);
  //  glEnable(GL_TEXTURE_2D);
  
  gReady = true;
  if (checkGLErrors()) cout << "Setup RC" << endl;
}

void shutdownRC(void)
{
  //g_shader.clean();
  if (checkGLErrors()) cout << "Shutdown RC" << endl;
}
#include <boost/lexical_cast.hpp>

void RenderScene(void)
{
  static uint32_t frameCounter = 0;
  static std::string fpsStr = "0";
  if (gReady)
  {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    float t = g_appTimer.timeElapsed_s();

    drawGeometry(t);
    if (frameCounter % 100 == 99)
    {
      fpsStr = boost::lexical_cast<std::string>((float)frameCounter / t);
    }
    if (gShowFps) gSystemFont->write(0, 0, fpsStr);

    g_font->write(0, 500, "The quick brown\nfox jumped\nover the lazy\ndog.");
    frameCounter += 1;
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
  g_projection = LPerspective(LRadians(40.0f), fAspect, 1.0f, 1000.0f);

  g_font->setViewport(w, h);
  gSystemFont->setViewport(w, h);
  gViewWidth = w;
  gViewHeight = h;
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
    //    g_shader.build();
  }
  else if (key == 'f' || key == 'F')
  {
    gShowFps = !gShowFps;
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
    SaveScreen("text");
  }
  glutPostRedisplay();
}

int main(int argc, char *argv[])
{
  srandom(1);
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowSize(1280, 720);
  glutCreateWindow("text");
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




