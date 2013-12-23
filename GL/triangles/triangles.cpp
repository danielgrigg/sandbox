/*
 *  \file triangles.cpp
 *  \brief triangles.cpp
 *  \author Daniel Grigg 
 *  \date 10/10/10.
 *  triangles
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

#undef glGenVertexArrays
#undef glDeleteVertexArrays
#undef glBindVertexArray
#define glGenVertexArrays glGenVertexArraysAPPLE
#define glDeleteVertexArrays  glDeleteVertexArraysAPPLE
#define glBindVertexArray	glBindVertexArrayAPPLE

using namespace std;

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

struct GeometryBatch
{
  enum
  {
    kPosOffset,
    kColorOffset
  };

  GLuint arrayObject;
  GLuint vertexObject;
  GLuint colorObject;
  GLuint primitiveType;
  vector<LPoint> ps;
  vector<LColor> cs;
  string name;

  void init(GLuint prim, const string &name)
  {
    primitiveType = prim;
    this->name = name;
    glGenVertexArrays(1, &arrayObject);
    glBindVertexArray(arrayObject);
    glGenBuffers(1, &vertexObject);
    glGenBuffers(1, &colorObject);
  }

  void bind()
  {
    glBindVertexArray(arrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, vertexObject);
    glBufferData(GL_ARRAY_BUFFER, sizeof(LPoint) * ps.size(), &ps[0], GL_STATIC_DRAW); //GL_DYNAMIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, colorObject);
    glBufferData(GL_ARRAY_BUFFER, sizeof(LColor) * cs.size(), &cs[0], GL_STATIC_DRAW); //GL_DYNAMIC_DRAW);

    glEnableVertexAttribArray(kPosOffset);
    glBindBuffer(GL_ARRAY_BUFFER, vertexObject);
    glVertexAttribPointer(kPosOffset, sizeof(LPoint)/sizeof(float), GL_FLOAT, GL_FALSE, 0, 0);

    glEnableVertexAttribArray(kColorOffset);
    glBindBuffer(GL_ARRAY_BUFFER, colorObject);
    glVertexAttribPointer(kColorOffset, sizeof(LColor)/sizeof(float), GL_FLOAT, GL_FALSE, 0, 0);
  }

  void draw() { glDrawArrays(primitiveType, 0, ps.size()); }
};

const int kNumBatches = 4;
GeometryBatch g_batches[kNumBatches];
GeometryBatch g_outlineBatches[kNumBatches];
int g_selectedBatch = 0;
bool g_drawOutline = false;
bool g_scissor = false;

void makeTriangle(GeometryBatch &batch, bool outline = false)
{
  float h = 2.0;
  batch.init(GL_TRIANGLE_STRIP, "triangle");
  batch.ps.resize(3);
  batch.ps[0] = LPoint(-h,-h,0);
  batch.ps[1] = LPoint(h,-h,0);
  batch.ps[2] = LPoint(0,h,0);
  batch.cs.assign(3, (outline ? LColor(0,0,0,1) : LColor(0,0,.8,1)));

}
void makeQuad(GeometryBatch &batch, bool outline = false)
{
  float h = 2.0f;
  batch.init(GL_TRIANGLE_STRIP, "quad");
  batch.ps.resize(4);
  batch.ps[0] = LPoint(-h,-h,0);
  batch.ps[1] = LPoint(h,-h,0);
  batch.ps[2] = LPoint(-h,h,0);
  batch.ps[3] = LPoint(h,h,0);
  batch.cs.assign(4, (outline ? LColor(0,0,0,1) : LColor(.9,.9,.9,1)));
}

void makeCurve(vector<LPoint> &p, float radius, float divisions)
{
  p.resize(divisions+1);
  for (int i = 0; i < p.size(); ++i)
  {
    float iNorm = (float)i / (float)divisions;
    float theta = 2.0f * c_LPI * iNorm;
    p[i] = LPoint(radius * cosf(theta), 0.0f, -radius * sinf(theta));
//    cout << "p[" << i << "]: " << p[i] << "\n";
  }
}

void makeRing(GeometryBatch &b, bool outline = false)
{
  float h = 0.5f;
  b.init(GL_TRIANGLE_STRIP, "ring");
  vector<LPoint> curve;
  makeCurve(curve, 3.0f, 32);
  b.ps.clear();
  for (int j = 0; j < curve.size(); ++j)
  {
    b.ps.push_back(curve[j] + LPoint(0,h,0));
    b.ps.push_back(curve[j] + LPoint(0,-h,0));
  }
  b.cs.assign(b.ps.size(), (outline ? LColor(0,0,0,1) : LColor(.85,.8,.4,1)));
  ostream_iterator<LColor> osi(cout, "\n");
  copy(b.cs.begin(), b.cs.end(), osi);
}

void makeFan(GeometryBatch &b, bool outline = false)
{
  const float h = -2.5f; 
  b.init(GL_TRIANGLE_FAN, "fan");
  vector<LPoint> curve;
  makeCurve(curve, 3.0f, 32);
  b.ps.clear();
  b.ps.push_back(LPoint(0, h, 0));
  for (int j = 0; j < curve.size(); ++j)
  {
    b.ps.push_back(curve[j] + LPoint(0,1,0));
  }
  b.cs.assign(b.ps.size(), (outline ? LColor(0,0,0,1) : LColor(.5,.9,.8,1)));
//  ostream_iterator<LPoint> osi(cout, "\n");
//  copy(b.ps.begin(), b.ps.end(), osi);
}

bool initShaders()
{
  vector<string> sources;
  sources.push_back("triangles.fp");
  sources.push_back("triangles.vp");

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
  int n = 0;
  makeRing(g_outlineBatches[n], true);
  makeRing(g_batches[n++]);

  makeQuad(g_outlineBatches[n], true); 
  makeQuad(g_batches[n++]); 

  makeTriangle(g_outlineBatches[n], true);
  makeTriangle(g_batches[n++]);

  makeFan(g_outlineBatches[n], true);
  makeFan(g_batches[n++]);
  if (noErrors()) cout << "Initialised geometry" << endl;
}

void drawGeometryOutline()
{
  glPolygonOffset(-1, -1);
  glEnable(GL_POLYGON_OFFSET_LINE);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glLineWidth(2.5f);

  g_outlineBatches[g_selectedBatch].bind();
  g_outlineBatches[g_selectedBatch].draw();
  
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glDisable(GL_POLYGON_OFFSET_LINE);
  glLineWidth(1.0f);
  glDisable(GL_BLEND);
  glDisable(GL_LINE_SMOOTH);
}

void drawGeometry()
{
  static float t = 0.0;
  t += 0.01f;

  glUseProgram(g_shader.program());
  glUniform1f(uniforms[kTime], t);
  LTransform M = LTranslate(.0f, 0, -9.0) * LRotateX(LRadians(30)) * LRotateY(LRadians(4.0f * t));
  LTransform MVP = g_projection * M;
  glUniformMatrix4fv(uniforms[kModelView], 1, GL_FALSE, M.asMatrix().asArray());
  glUniformMatrix4fv(uniforms[kMVP], 1, GL_FALSE, MVP.asMatrix().asArray());

  g_batches[g_selectedBatch].bind();
  g_batches[g_selectedBatch].draw();
  if (g_drawOutline) drawGeometryOutline();
  noErrors();
}

void setupRC()
{
  glClearColor(0.5f, 0.5f, 0.5f,1.0f);
  if (!initShaders() )
  {
    cout << "init shaders error\n";
    return;
  }

  uniforms[kModelView] = glGetUniformLocation(g_shader.program(), "mvMatrix");
  uniforms[kMVP] = glGetUniformLocation(g_shader.program(), "mvpMatrix");
  uniforms[kTime] = glGetUniformLocation(g_shader.program(), "uTime");
  initGeometry();
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
  glScissor(w/4, h/4, w/2 + w/4, h/2 + h/4);
  cout << "Scissor window at (" << w/4 << ", " << h/4 << ", " << w/2+w/4 << ", " << h/2+h/4 << ")";

  fAspect = (GLfloat)w/(GLfloat)h;
  g_projection = LPerspective(LRadians(40.0f), fAspect, 1.0f, 100.0f);
}

void KeyPressFunc(unsigned char key, int x, int y)
{
	if(key == 'n' || key == 'N' || key == 0x20)
  {
    g_selectedBatch = (g_selectedBatch + 1) % kNumBatches;
    cout << "Switching to " << g_batches[g_selectedBatch].name << " batch.\n";
  }
  else if (key == 'o' || key == 'O')
  {
    g_drawOutline = !g_drawOutline;
    cout << (g_drawOutline ? "drawing" : "not drawing") << " outline\n";
  }
  else if (key == 's' || key == 'S')
  {
    g_scissor = !g_scissor;
    g_scissor ? glEnable(GL_SCISSOR_TEST) : glDisable(GL_SCISSOR_TEST);
    cout << (g_scissor ? "scissoring screen centre" : "not scissoring") << "\n";
  }

  else if (key == 'b' || key == 'B')
  {
    g_shader.build();
  }
  else if (key == 'c' || key == 'C')
  {
    static bool cullFace = false;
    cullFace = !cullFace;
    cullFace ? glEnable(GL_CULL_FACE) : glDisable(GL_CULL_FACE); 
    cout << "Face culling " << (cullFace ? "enabled" : "disabled") << "\n";
  }
  else if (key == 'f' || key == 'F')
  {
    static bool front = false;
    front = !front;
    front ? glCullFace(GL_FRONT) : glCullFace(GL_BACK);
    cout << "Culling " << (front ? "front" : "back") << "faces\n";
  }
  else if (key == 'd' || key == 'D')
  {
    static bool depthTest = false;
    depthTest = !depthTest;
    depthTest ? glEnable(GL_DEPTH_TEST) : glDisable(GL_DEPTH_TEST);
    cout << "Depth Test " << (depthTest ? "enabled" : "disabled") << "\n";
  }
  else if (key == 'm' || key == 'M')
  {
    static bool multisample = false;
    multisample = !multisample;
    multisample ? glEnable(GL_MULTISAMPLE) : glDisable(GL_MULTISAMPLE);
    glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    cout << "Multisampling " << (multisample ? "enabled" : "disabled") << "\n";
    noErrors();
  }

  else if (key == 'p' || key == 'P')
  {
    static GLuint modes[] = {GL_FILL, GL_LINE, GL_POINT};
    static const char * modeStrings[]  = {"fill", "line", "point"};
    static int modeIdx = 0;
    glPointSize(3.0f);
    modeIdx = (modeIdx + 1) % 3;
    glPolygonMode(GL_FRONT_AND_BACK, modes[modeIdx]);
    cout << modeStrings[modeIdx] << " polygon mode\n";
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
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH | GLUT_MULTISAMPLE);
  glutInitWindowSize(1280, 720);
  glutCreateWindow("triangles");
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




