/*
 *  \file cubemaps.cpp
 *  \brief cubemaps.cpp
 *  \author Daniel Grigg 
 *  \date 10/10/10.
 *  cubemaps
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
  kSkyColorMap,
  kOutlineMVP,
  kReflectMV,
  kReflectMVP,
  kReflectNormalMatrix,
  kReflectSkyColorMap,
  kReflectInvView, 
  kUniformMax
};

GLuint uniforms[kUniformMax];
bool gReady = false;
LTransform g_projection;
LShader g_skyShader;
LShader g_reflectShader;
static bool g_paused = false;
static LStopwatch g_appTimer;
static GLuint g_skyTexture;
static bool g_drawOutline = false;

static LShader g_outlineShader;
std::vector<GeometryBatch> g_batches;
int g_selectedBatch = 0;
static GeometryBatch g_skyBatch;

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
  bool result = true;
  result = result && loadShaderPair(g_skyShader, "sky_box"); 
  result = result && loadShaderPair(g_reflectShader, "cubemap_reflect"); 
  result = result && loadShaderPair(g_outlineShader, "outline");
  g_skyTexture = loadCubeMapFromFaceFiles("../Resources/sky");
  glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS);

  if (checkGLErrors()) cout << "Initialised shaders" << endl;
  return result;
}

void initGeometry()
{
  int n = 0;
  LColor c(1.0f, 0.0f, 0.0f, 1.0f);
  g_batches.resize(3);
  makeCube(g_skyBatch, 20.0f, false, false);
  makeSphere(g_batches[n++], 2.0f, 32, true, false);
  makeQuad(g_batches[n++], 2.0f, 2.0, true, false);
  makeRing(g_batches[n++], 2.0f, 2.0f, 32, true, false);
  if (checkGLErrors()) cout << "Initialised geometry" << endl;
}

void drawGeometryOutline(const LTransform &MVP)
{
  glUseProgram(g_outlineShader.program());
  glUniformMatrix4fv(uniforms[kOutlineMVP], 1, GL_FALSE, MVP.array());

  glPolygonOffset(-1, -1);
  glEnable(GL_POLYGON_OFFSET_LINE);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glLineWidth(2.5f);

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
  glUseProgram(g_skyShader.program());
  LTransform M = LIdentity();

  // Spin around the object
  float u = 0.1f * t;
  float r = 9.0f;
  LTransform V =  LRotateY(-u + c_LPI/2.0f) * 
    LTranslate(r * cos(u), 0.0f, -r * sin(u));
  LTransform MV = V * M;
  LTransform MVP = g_projection * MV;

//  cout << LDegrees(-u + c_LPI/2.0f) << ", x=" << r * cos(u) << ", z=" << -r*sin(u) << endl;

  glUniformMatrix4fv(uniforms[kModelView], 1, GL_FALSE, MV.array());
  glUniformMatrix4fv(uniforms[kMVP], 1, GL_FALSE, MVP.array());
  glUniform1i(uniforms[kSkyColorMap], 0);

  g_skyBatch.bind();
  g_skyBatch.draw();


  glUseProgram(g_reflectShader.program());
  LTransform N(MV.inverse().matrix().transpose());
  LTransform VInv(V.inverse());
  glUniformMatrix4fv(uniforms[kReflectNormalMatrix], 1, GL_FALSE, N.array());
  glUniformMatrix4fv(uniforms[kReflectMV], 1, GL_FALSE, MV.array());
  glUniformMatrix4fv(uniforms[kReflectMVP], 1, GL_FALSE, MVP.array());
  glUniform1i(uniforms[kReflectSkyColorMap], 0);
  glUniformMatrix4fv(uniforms[kReflectInvView], 1, GL_FALSE, VInv.array());
  if (g_batches.size() > 0)
  {
    g_batches[g_selectedBatch].bind();
    g_batches[g_selectedBatch].draw();
  }

  if (g_drawOutline) drawGeometryOutline(MVP);
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
  uniforms[kOutlineMVP] = glGetUniformLocation(g_outlineShader.program(), "mvpMatrix");
  uniforms[kModelView] = glGetUniformLocation(g_skyShader.program(), "mvMatrix");
  uniforms[kMVP] = glGetUniformLocation(g_skyShader.program(), "mvpMatrix");
  uniforms[kSkyColorMap] = glGetUniformLocation(g_skyShader.program(), "skyColorMap");
 // uniforms[kTime] = glGetUniformLocation(g_skyShader.program(), "uTime");

  uniforms[kReflectNormalMatrix] = glGetUniformLocation(g_reflectShader.program(), "normalMatrix");
  uniforms[kReflectMV] = glGetUniformLocation(g_reflectShader.program(), "mvMatrix");
  uniforms[kReflectMVP] = glGetUniformLocation(g_reflectShader.program(), "mvpMatrix");
  uniforms[kReflectInvView] = glGetUniformLocation(g_reflectShader.program(), "invCamera");
  uniforms[kReflectSkyColorMap] = glGetUniformLocation(g_reflectShader.program(), "skyColorMap");
  uniforms[kReflectInvView] = glGetUniformLocation(g_reflectShader.program(), "invCamera");

  initGeometry();

  glEnable(GL_DEPTH_TEST);

  gReady = true;
  if (checkGLErrors()) cout << "Setup RC" << endl;
}

void shutdownRC(void)
{
  g_skyShader.clean();
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
    g_skyShader.build();
  }
  else if (key == 'o' || key == 'O')
  {
    g_drawOutline = !g_drawOutline;
    cout << (g_drawOutline ? "drawing" : "not drawing") << " outline.\n";
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
    SaveScreen("cubemaps");
  }
  glutPostRedisplay();
}

int main(int argc, char *argv[])
{
  gReady = false;
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowSize(1280, 720);
  glutCreateWindow("cubemaps");
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




