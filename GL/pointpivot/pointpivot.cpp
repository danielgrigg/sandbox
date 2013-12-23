/*
 *  \file pointpivot.cpp
 *  \brief pointpivot.cpp
 *  \author Daniel Grigg 
 *  \date 10/10/10.
 *  pointpivot
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
LShader g_shader;
static bool g_paused = false;
static LStopwatch g_appTimer;
static GLuint g_starColorMap;
static int g_selectedPoint = 0;

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
  bool result = true;
  result = result && loadShaderPair(g_shader, "pointpivot"); 
  if (checkGLErrors()) cout << "Initialised shaders" << endl;

  g_starColorMap = loadTextureFromFile("../Resources/star.tga", GL_CLAMP_TO_EDGE, 
      GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR);

  return result;
}


float randomOne()
{
  return (float)rand() / (float)RAND_MAX;
}

LPoint makePointPosition()
{
  return LPoint(-30.0f + 60.0f * randomOne(),
                -10.0f + 20.0f * randomOne(),
                -100.0f);
}

LColor makePointColor()
{
	LColor colors[4] = {LColor(1.0f, 1.0f, 1.0f, 1.0f), // White
                          LColor(0.67f, 0.68f, 0.82f, 1.0f), // Blue Stars
                          LColor(1.0f, 0.5f, 0.5f, 1.0f), // Reddish
	                        LColor(1.0f, 0.82f, 0.65f, 1.0f)}; // Orange
  float x = randomOne();
  if (x <= 0.01f) return colors[3];
  if (x <= 0.02f) return colors[2];
  if (x <= 0.2f) return colors[1];
  return colors[0];
}

void makePointField(GeometryBatch &batch)
{
  batch.init(GL_POINTS, "pointField");
  batch.ps.resize(100);
  std::generate(batch.ps.begin(), batch.ps.end(), makePointPosition);
  batch.cs.resize(batch.ps.size());
  std::generate(batch.cs.begin(), batch.cs.end(), makePointColor);
}

void initGeometry()
{
  if (!checkGLErrors()) return;
  int n = 0;
  LColor c(1.0f, 0.0f, 0.0f, 1.0f);
  g_batches.resize(1);
  makePointField(g_batches[0]);
  if (checkGLErrors()) cout << "Initialised geometry" << endl;
}

void drawGeometry()
{
  float t = g_appTimer.timeElapsed_s();

	glEnable(GL_PROGRAM_POINT_SIZE);
 // glPointSize(8.0f);
  glEnable(GL_POINT_SPRITE);
  glEnable(GL_BLEND);
	glBlendFunc(GL_ONE, GL_ONE);

  glUseProgram(g_shader.program());
  glUniform1f(uniforms[kTime], t);
  //LTransform M = LTranslate(.0f, 0.0, 90.0*(.5 + .5 * sin(.5f*t)));

  LPoint p = g_batches[0].ps[g_selectedPoint];
  LTransform M = LRotateZ(.5 * t) * LTranslate(-p.x(), -p.y(), 30.0);
//    LTranslate(.0f, 0.0, 90.0*(.5 + .5 * sin(.5f*t)));
  
  LTransform MVP = g_projection * M;
  glUniformMatrix4fv(uniforms[kModelView], 1, GL_FALSE, M.asMatrix().asArray());
  glUniformMatrix4fv(uniforms[kMVP], 1, GL_FALSE, MVP.asMatrix().asArray());

  g_batches[g_selectedBatch].bind();
  g_batches[g_selectedBatch].draw();
  checkGLErrors();
}

void setupRC()
{
  glClearColor(0.0, 0.0, 0.0,1.0f);
  if (!initShaders() )
  {
    cout << "init shaders error\n";
    return;
  }

  uniforms[kModelView] = glGetUniformLocation(g_shader.program(), "mvMatrix");
  uniforms[kMVP] = glGetUniformLocation(g_shader.program(), "mvpMatrix");
  uniforms[kTime] = glGetUniformLocation(g_shader.program(), "uTime");
  initGeometry();

//  glEnable(GL_DEPTH_TEST);

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
  g_projection = LPerspective(LRadians(35.0f), fAspect, 1.0f, 1000.0f);
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
  else if (key == 'c' || key == 'C')
  {
    LAssert(g_batches.size() > 0);
    g_selectedPoint = (g_selectedPoint + 1) % g_batches[0].ps.size();
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
    SaveScreen("pointpivot");
  }
  glutPostRedisplay();
}

int main(int argc, char *argv[])
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowSize(1280, 720);
//  glutInitWindowSize(512, 288);
  glutCreateWindow("pointpivot");
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




