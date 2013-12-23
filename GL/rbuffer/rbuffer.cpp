/*
 *  \file rbuffer.cpp
 *  \brief rbuffer.cpp
 *  \author Daniel Grigg 
 *  \date 10/10/10.
 *  rbuffer
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
#include <boost/function.hpp>
#include <map>
#include "Framebuffer.h"

using namespace std;

// GLenum windowBuff [] = {GL_FRONT_LEFT };
// glDrawBuffers(1, windowBuff);
//
const std::string mediaPath = "../Resources";

enum 
{
  kModelView,
  kMVP,
  kTime,
  kUniformMax
};

GLuint uniforms[kUniformMax];
bool gReady = false;
LTransform g_projection;
LShader g_shader;
static bool g_paused = false;
static LStopwatch g_appTimer;
typedef std::tr1::shared_ptr<LFont> LFontPtr;
LFontPtr gSystemFont;
std::vector<GeometryBatch> g_batches;
int g_selectedBatch = 0;
static bool gShowFps = true;
ViewFramebuffer gViewFramebuffer(1280, 720);

static Framebuffer gFB;

bool initShaders()
{
  vector<string> sources;
  sources.push_back("rbuffer.fp");
  sources.push_back("rbuffer.vp");

  vector<std::pair<string,int> > attributes;
  attributes.push_back(std::make_pair("vVertexPos", (int)GeometryBatch::kPosOffset));
  attributes.push_back(std::make_pair("vColor", (int)GeometryBatch::kColorOffset));
  g_shader.setSources(sources, attributes);
  bool result = g_shader.build();
  cout << "Shader: " << g_shader << endl;
  if (checkGLErrors()) cout << "Initialised shaders" << endl;
  return result;
}

bool initContent()
{
  const int dpi = 72;
  gSystemFont = LFontPtr(new LFont("Consolas", 20, dpi, "../content/"));
  initShaders();


  if (!gFB.init("my-fb", 
        gViewFramebuffer.width(), 
        gViewFramebuffer.height(),
        makeRenderbufferD1C3)) return false;

  if (!checkGLErrors()) return false;
  return true;
}

bool initGeometry()
{
  if (!checkGLErrors()) return false;
  int n = 0;
  LColor c(127,127,255,255);
  g_batches.resize(1);
  makeQuad(g_batches[n++], 2.0f, 2.0f, false, true, &c);
  if (!checkGLErrors()) 
  {
    cout << "error init geometry" << endl;
    return false;
  }
  cout << "Initialised geometry" << endl;
  return true;
}

void drawGeometry(float t)
{
  glUseProgram(g_shader.program());
  glUniform1f(uniforms[kTime], t);
  LTransform M = LTranslate(.0f, 0.4, -8.0) *
    LRotateX(LRadians(30)) * LRotateY(LRadians(7.0f * t));
  LTransform MVP = g_projection * M;
  glUniformMatrix4fv(uniforms[kModelView], 1, GL_FALSE, M.array());
  glUniformMatrix4fv(uniforms[kMVP], 1, GL_FALSE, MVP.array());

  g_batches[g_selectedBatch].bind();
  g_batches[g_selectedBatch].draw();
}

void setupRC()
{
  glClearColor(0.15f, 0.15f, 0.15f,1.0f);
  if (!initContent() )
  {
    cout << "init content error\n";
    return;
  }

  uniforms[kModelView] = glGetUniformLocation(g_shader.program(), "mvMatrix");
  uniforms[kMVP] = glGetUniformLocation(g_shader.program(), "mvpMatrix");
  uniforms[kTime] = glGetUniformLocation(g_shader.program(), "uTime");
  if (!initGeometry()) { return; }

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
  static uint32_t frameCounter = 0;
  static std::string fpsStr = "0";
  if (gReady)
  {
    glGetError();
    glClearColor(0.15f, 0.15f, 1.0,1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    float t = g_appTimer.timeElapsed_s();

    gFB.useWriting();
    glClearColor(0.15f, 0.15f, 0.15,1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    drawGeometry(t);
    gViewFramebuffer.useWriting();
    gFB.useReading(1); 
    glBlitFramebuffer(0, 0, gFB.width(), gFB.height(),
        0, 0, gViewFramebuffer.width()/2, gViewFramebuffer.height()/2,
        GL_COLOR_BUFFER_BIT, GL_LINEAR);
    gFB.useReading(2);
    glBlitFramebuffer(0, 0, gFB.width(), gFB.height(),
        0, gViewFramebuffer.height()/2, 
        gViewFramebuffer.width()/2, gViewFramebuffer.height(),
        GL_COLOR_BUFFER_BIT, GL_LINEAR);

    gSystemFont->write(2*gViewFramebuffer.width()/3, gViewFramebuffer.height() /2,
        "The rotating quads on the left are\ndrawn to an offscreen-buffer\n"
        "and blitted to the left of the\nview. The top and bottom source\n"
        "independent color buffers. This\ntext is drawn directly to the view.");
    if (frameCounter % 100 == 99)
    {
      fpsStr = boost::lexical_cast<std::string>((float)frameCounter / t);
    }
    if (gShowFps) gSystemFont->write(0, 0, fpsStr);
    frameCounter += 1;

    checkGLErrors();
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

  gSystemFont->setViewport(w, h);
  gViewFramebuffer = ViewFramebuffer(w, h);
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
    SaveScreen("rbuffer");
  }
  glutPostRedisplay();
}

int main(int argc, char *argv[])
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowSize(gViewFramebuffer.width(), gViewFramebuffer.height());
  glutCreateWindow("rbuffer");
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



