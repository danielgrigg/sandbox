/*
 *  \file primitives.cpp
 *  \brief primitives.cpp
 *  \author Daniel Grigg 
 *  \date 10/10/10.
 *  primitives
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

#undef glGenVertexArrays
#undef glDeleteVertexArrays
#undef glBindVertexArray
#define glGenVertexArrays glGenVertexArraysAPPLE
#define glDeleteVertexArrays  glDeleteVertexArraysAPPLE
#define glBindVertexArray	glBindVertexArrayAPPLE

using namespace std;

enum
{
  kPoints,
  kLines,
  kLineStrip,
  kLineLoop,
  kPrimitiveModeMax
};

const uint32_t modeMap[] = {GL_POINTS, GL_LINES, GL_LINE_STRIP, GL_LINE_LOOP, 0};

uint32_t g_primitiveMode = kPoints;
enum
{
  kPosOffset,
  kColorOffset
};

const char * kAttribs[2] = {
  "vVertexPos", "vColor"
};
const std::string mediaPath = "../Resources";

enum 
{
  kModelView,
  kMVP,
  kTime,
  kUniformMax
};

GLuint uniforms[kUniformMax];
GLuint g_colorArray;
GLuint g_vertexArray;
GLuint g_VAO;
bool gReady = false;
LTransform g_projection;
LShader g_shader;

bool noErrors()
{
  GLenum e = glGetError();
  if (e != GL_NO_ERROR)
  {
    cout << "GL error: " << e << "\n";
    return false;
  }
  return true;
}

void uploadGeometry()
{
  if (!noErrors()) return;
  const int n = 23;
  LPoint v[n] = {LPoint(38,99,0),
                 LPoint(10,98,0),
                 LPoint(5,90,0),
                 LPoint(25,92,0),
                 LPoint(30,95,0),
                 LPoint(32,90,0),
                 LPoint(35,85,0),
                 LPoint(32,82,0),
                 LPoint(30,80,0),
                 LPoint(27,65,0),
                 LPoint(25,50,0),
                 LPoint(25,47,0),
                 LPoint(25,45,0),
                 LPoint(45,30,0),
                 LPoint(65,15,0),
                 LPoint(73,20,0),
                 LPoint(80,25,0),
                 LPoint(82,32,0),
                 LPoint(85,40,0),
                 LPoint(87,50,0),
                 LPoint(60,60,0),
                 LPoint(60,65,0),
                 LPoint(60,70,0)};
  vector<LColor> c(n, LColor(0,0,0,1));
	glBindVertexArray(g_VAO);
  glBindBuffer(GL_ARRAY_BUFFER, g_vertexArray);
  glBufferData(GL_ARRAY_BUFFER, sizeof(LPoint) * n, &v[0], GL_DYNAMIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, g_colorArray);
  glBufferData(GL_ARRAY_BUFFER, sizeof(LColor) * n, &c[0], GL_DYNAMIC_DRAW);
  
  glEnableVertexAttribArray(kPosOffset);
  glBindBuffer(GL_ARRAY_BUFFER, g_vertexArray);
  glVertexAttribPointer(kPosOffset, sizeof(LPoint)/sizeof(float), GL_FLOAT, GL_FALSE, 0, 0);
  
  glEnableVertexAttribArray(kColorOffset);
  glBindBuffer(GL_ARRAY_BUFFER, g_colorArray);
  glVertexAttribPointer(kColorOffset, sizeof(LColor)/sizeof(float), GL_FLOAT, GL_FALSE, 0, 0);
  noErrors();
}

bool initShaders()
{
  vector<string> sources;
  sources.push_back("primitives.fp");
  sources.push_back("primitives.vp");

  vector<string> attributes;
  attributes.push_back("vVertexPos");
  attributes.push_back("vColor");
  g_shader.setSources(sources, attributes);
  bool result = g_shader.build();
  cout << "Shader: " << g_shader << endl;
  if (noErrors()) cout << "Initialised shaders" << endl;
  return result;
}

void initGeometry()
{
  if (!noErrors()) return;
  glGenVertexArrays(1, &g_VAO);
  glBindVertexArray(g_VAO);
  glGenBuffers(1, &g_vertexArray);
  glGenBuffers(1, &g_colorArray);

  if (noErrors()) cout << "Initialised geometry" << endl;
}

void drawGeometry()
{
  static float t = 0.0;
  t += 0.01f;
  uploadGeometry();

  glUseProgram(g_shader.program());
  glBindVertexArray(g_VAO);
  glUniform1f(uniforms[kTime], t);
  LTransform M = LTranslate(.0f, 0, 0.0);
  LTransform MVP = g_projection * M;
  glUniformMatrix4fv(uniforms[kModelView], 1, GL_FALSE, M.asMatrix().asArray());
  glUniformMatrix4fv(uniforms[kMVP], 1, GL_FALSE, MVP.asMatrix().asArray());
  glDrawArrays(modeMap[g_primitiveMode], 0, 23);
}

void setupRC()
{
  glClearColor(0.7f, 0.7f, 0.7f,1.0f);
  if (!initShaders() )
  {
    cout << "init shaders error\n";
    return;
  }

  uniforms[kModelView] = glGetUniformLocation(g_shader.program(), "mvMatrix");
  uniforms[kMVP] = glGetUniformLocation(g_shader.program(), "mvpMatrix");
  uniforms[kTime] = glGetUniformLocation(g_shader.program(), "uTime");
  initGeometry();

  glPointSize(3.0f);
  glLineWidth(2.0f);
  gReady = true;
  if (noErrors()) cout << "Setup RC" << endl;
}

void shutdownRC(void)
{
  g_shader.clean();
  if (noErrors()) cout << "Shutdown RC" << endl;
}

void RenderScene(void)
{
  glClear(GL_COLOR_BUFFER_BIT);
  if (gReady)
  {
    drawGeometry();
    if (0)
    {
      glUseProgram(0);
      glBindVertexArray(0);
      glBegin(GL_TRIANGLES);
      glVertex3f(-.5, 0, 0);
      glVertex3f(.5, 0, 0);
      glVertex3f(0, .5f, 0);
      glEnd();
    }

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
  //g_projection = LPerspective(LRadians(40.0f), fAspect, 1.0f, 100.0f);
  g_projection = LOrtho(0,100,0,100);
}

void KeyPressFunc(unsigned char key, int x, int y)
{
	if(key == 'n' || key == 'N' || key == 0x20)
  {
    g_primitiveMode = (g_primitiveMode + 1) % kPrimitiveModeMax;
  }
  else if (key == '[' || key == ']')
  {
    static float pointSize = 3.0f;
    GLfloat sizes[2];
    GLfloat step;
    glGetFloatv(GL_POINT_SIZE_RANGE, sizes);
    glGetFloatv(GL_POINT_SIZE_GRANULARITY, &step);
    pointSize = key == '[' ? 
      std::max(sizes[0], pointSize - step) : std::min(sizes[1], pointSize + step);
    glPointSize(pointSize);
    cout << "PointSize: " << pointSize << " (" << sizes[0] << ", " << sizes[1] << ", " << step << ")\n";
  }
  else if (key == ';' || key == '\'')
  {
    static float lineSize = 2.0f;
    const float step = 0.125f;
    lineSize = key == ';' ? 
      std::max(1.0f, lineSize - step) : std::min(10.0f, lineSize + step);
    glLineWidth(lineSize);
    cout << "LineWidth: " << lineSize << " (" << 1 << ", " << 10 << ", " << step << ")\n";
  }
  else if (key == 'b' || key == 'B')
  {
    g_shader.build();
  }
  glutPostRedisplay();
}

int main(int argc, char *argv[])
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
  glutInitWindowSize(1280, 720);
  glutCreateWindow("TEMPLATE");
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
  glutMainLoop();
  shutdownRC();
  return 0;
}




