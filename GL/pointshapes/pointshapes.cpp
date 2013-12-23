/*
 *  \file pointshapes.cpp
 *  \brief pointshapes.cpp
 *  \author Daniel Grigg 
 *  \date 10/10/10.
 *  pointshapes
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
  kTime,
  kUniformMax
};

GLuint uniforms[kUniformMax];
bool gReady = false;
LTransform g_projection;
vector<LShader> g_shaders;
static bool g_paused = false;
static LStopwatch g_appTimer;

std::vector<GeometryBatch> g_batches;
int g_selectedBatch = 0;

bool loadShaderPair(LShader &shader, const std::string &name)
{
  const std::string fp = name + ".fp";
  const std::string vp = name + ".vp";
  vector<string> sources;
  sources.push_back(fp);
  sources.push_back(vp);

  vector<std::pair<string,int> > attributes;
  attributes.push_back(std::make_pair("vVertexPos", (int)GeometryBatch::kPosOffset));
  attributes.push_back(std::make_pair("vColor", (int)GeometryBatch::kColorOffset));

  shader.setSources(sources, attributes);
  bool result = shader.build();
  cout << "Shader: " << shader << endl;
  return result;
}


bool initShaders()
{
  g_shaders.resize(1);
  bool result = true;
  int n = 0;
  result = result && loadShaderPair(g_shaders[n++], "flower"); 

  if (checkGLErrors()) cout << "Initialised shaders" << endl;
  return result;
}

void makePoint(GeometryBatch &batch, const std::string &name)
{
  batch.init(GL_POINTS, name);
  batch.ps.resize(3);
  batch.ps[0] = LPoint(0,.3,0);
  batch.ps[1] = LPoint(0.5,-.3,0);
  batch.ps[2] = LPoint(-.5,-.3,0);
  batch.cs.resize(batch.ps.size());
  batch.cs[0] = LColor(-.6,1,1,1);
  batch.cs[1] = LColor(.6,1,1,1);
  batch.cs[2] = LColor(1,1,1,1);
}

void initGeometry()
{
  if (!checkGLErrors()) return;
  int n = 0;
  LColor c(1.0f, 0.0f, 0.0f, 1.0f);
  g_batches.resize(1);
  makePoint(g_batches[n++], "flower");
  if (checkGLErrors()) cout << "Initialised geometry" << endl;
}

void drawGeometry()
{
  float t = g_appTimer.timeElapsed_s();

	glEnable(GL_PROGRAM_POINT_SIZE);
 // glPointSize(8.0f);
  glEnable(GL_POINT_SPRITE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glUseProgram(g_shaders[0].program());
  glUniform1f(uniforms[kTime], t);
  LTransform M = LTranslate(.0f, 0.0, -2.0);
  LTransform MVP = g_projection * M;
  glUniformMatrix4fv(uniforms[kModelView], 1, GL_FALSE, M.asMatrix().asArray());
  glUniformMatrix4fv(uniforms[kMVP], 1, GL_FALSE, MVP.asMatrix().asArray());

  g_batches[g_selectedBatch].bind();
  g_batches[g_selectedBatch].draw();
  checkGLErrors();
}

void setupRC()
{
  glClearColor(0.15f, 0.15f, 0.15f,1.0f);
  if (!initShaders() )
  {
    cout << "init shaders error\n";
    return;
  }

  uniforms[kModelView] = glGetUniformLocation(g_shaders[0].program(), "mvMatrix");
  uniforms[kMVP] = glGetUniformLocation(g_shaders[0].program(), "mvpMatrix");
  uniforms[kTime] = glGetUniformLocation(g_shaders[0].program(), "uTime");
  initGeometry();

//  glEnable(GL_DEPTH_TEST);
  glEnable(GL_MULTISAMPLE);

  gReady = true;
  if (checkGLErrors()) cout << "Setup RC" << endl;
}

void shutdownRC(void)
{
  g_shaders[0].clean();
  if (checkGLErrors()) cout << "Shutdown RC" << endl;
}

void RenderScene(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  if (gReady)
  {
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
    g_shaders[0].build();
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
    SaveScreen("pointshapes");
  }
  glutPostRedisplay();
}

int main(int argc, char *argv[])
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH | GLUT_MULTISAMPLE);
  glutInitWindowSize(1280, 720);
  glutCreateWindow("pointshapes");
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




