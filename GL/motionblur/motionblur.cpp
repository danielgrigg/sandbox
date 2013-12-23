/*
 *  \file motionblur.cpp
 *  \brief motionblur.cpp
 *  \author Daniel Grigg 
 *  \date 10/10/10.
 *  motionblur
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
#include <boost/format.hpp>
#include "../shared/ObjImport.hpp"
#include "../shared/ObjManip.hpp"

using namespace std;

const std::string mediaPath = "../Resources";

enum 
{
  kModelView,
  kMVP,
  kNormalMatrix,
  kBlurMVP,
  kLightPosition,
  kTime,
  kUniformMax
};

GLuint uniforms[kUniformMax];
bool gReady = false;
LTransform g_projection;
LShader g_shader;
LShader gBlurShader;
static bool g_paused = false;
static LStopwatch g_appTimer;
typedef std::tr1::shared_ptr<LFont> LFontPtr;
LFontPtr gSystemFont;
std::vector<GeometryBatch> g_batches;
GeometryBatch gScreenQuad;
int g_selectedBatch = 0;
static bool gShowFps = true;
static uint32_t gViewWidth = 0;
static uint32_t gViewHeight = 0;
static vector<uint8_t> gPixelData;
LTransform gScreenProjection;

GLuint pixBuffObjs[1];
const uint32_t kNumBlurTextures = 10;
uint32_t gBlurTextures[kNumBlurTextures];
GLuint gBlurMaps[kNumBlurTextures];
uint32_t gCurrentBlurTarget = 0;

bool importIndexedObj(GeometryBatch& batch, const char* name)
{
  ObjImport obj;
  if (!obj.import(name))
  {
    std::cerr << "Error importing " << name << std::endl;
    return false;
  }
  IndexedTriangleBatch indexedTriBatch;
  makeIndexedTriangleBatchFromObjImport(obj, indexedTriBatch);

  batch.init(GL_TRIANGLES, name);
  batch.ps.resize(indexedTriBatch.vs.size());
  batch.ns.resize(indexedTriBatch.vs.size());
  //  batch.uvs[0].resize(indexedTriBatch.vs.size());

  for (int i = 0; i < indexedTriBatch.vs.size(); ++i)
  {
    const vec3f& p = indexedTriBatch.vs[i];
    const vec3f& n = indexedTriBatch.ns[i];
    batch.ps[i] = LPoint(p.x, p.y, p.z);
    batch.ns[i] = LNormal(n.x, n.y, n.z);
  }
  batch.elements.resize(indexedTriBatch.is.size());
  std::copy(indexedTriBatch.is.begin(), indexedTriBatch.is.end(), batch.elements.begin());

  std::cout << "Imported " << name << " (" << batch.ps.size() 
    << " vertices, " << batch.elements.size()/3 << " triangles)\n";
  return true;
}



// | 0 | 1 | 2 | 3 | 4 | 5 |
//
//   0  -5  -4  -3  -2  -1
//  -1   0  -5  -4  -3  -2
//  -2  -1   0  -5  -4  -3
uint32_t GetBlurTarget(uint32_t age)
{
  uint32_t index = (kNumBlurTextures + (gCurrentBlurTarget - age)) % kNumBlurTextures;
  return index;
}

bool initShaders()
{
  vector<string> sources;
  sources.push_back("../content/diffuse_point.fp");
  sources.push_back("../content/diffuse_point.vp");

  vector<std::pair<string,int> > attributes;
  attributes.push_back(std::make_pair("vVertexPos", (int)GeometryBatch::kPosOffset));
  attributes.push_back(std::make_pair("vNormal", (int)GeometryBatch::kNormalOffset));
  g_shader.setSources(sources, attributes);
  bool result = g_shader.build();
  cout << "Shader: " << g_shader << endl;

  sources[0] = "blur.fp";
  sources[1] = "blur.vp";
  attributes[0] = (std::make_pair("vVertexPos", (int)GeometryBatch::kPosOffset));
  attributes[1] = (std::make_pair("vTexCoord", (int)GeometryBatch::kUV0Offset));
  gBlurShader.setSources(sources, attributes);
  result = result && gBlurShader.build();
  cout << "Blur Shader: " << gBlurShader << endl;
  if (checkGLErrors()) cout << "Initialised shaders" << endl;

  return result;
}

bool initContent()
{
  const int dpi = 72;
  gSystemFont = LFontPtr(new LFont("Consolas", 20, dpi, "../content/"));
  initShaders();

  glGenTextures(kNumBlurTextures, gBlurTextures);
  uint32_t pixelDataSize = gViewWidth * gViewHeight * 3 * sizeof(uint8_t);
  gPixelData.resize(pixelDataSize, 0);
  for (uint32_t i = 0; i < kNumBlurTextures; ++i)
  {
    glActiveTexture(GL_TEXTURE0+i);
    glBindTexture(GL_TEXTURE_RECTANGLE, gBlurTextures[i]);
    glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_RECTANGLE, 0, GL_RGB, gViewWidth, gViewHeight, 0, 
        GL_RGB, GL_UNSIGNED_BYTE, NULL);
  }
  glGenBuffers(1, pixBuffObjs);
  glBindBuffer(GL_PIXEL_PACK_BUFFER, pixBuffObjs[0]);
  glBufferData(GL_PIXEL_PACK_BUFFER, pixelDataSize, NULL, GL_DYNAMIC_COPY);
  glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
  if (!checkGLErrors()) return false;
  cout << "Initialised content" << endl;
  return true;
}

bool initGeometry()
{
  if (!checkGLErrors()) return false;
  int n = 0;
  LColor c(127,127,255,255);
  g_batches.resize(1);
//  makeCube(g_batches[n++], 1.0f, true, false, NULL);
  //makeSphere(g_batches[n++], 1.0f, 32, true, false);

  LStopwatch timer;
  if (!importIndexedObj(g_batches[n++], "../content/pipe_s.obj")) return false;
  double dt = timer.timeElapsed_s();
  std::cout << "Imported models in " << dt << "s\n";

  makeRect(gScreenQuad, gViewWidth, gViewWidth, false, true, NULL);
  if (!checkGLErrors())
  {
    cout << "init geometry error\n";
    return false;
  }
  cout << "Initialised geometry" << endl;
  return true;
}

void setupUniforms()
{
  uniforms[kModelView] = glGetUniformLocation(g_shader.program(), "mvMatrix");
  uniforms[kMVP] = glGetUniformLocation(g_shader.program(), "mvpMatrix");
  uniforms[kBlurMVP] = glGetUniformLocation(gBlurShader.program(), "mvpMatrix");
  uniforms[kTime] = glGetUniformLocation(g_shader.program(), "uTime");
  uniforms[kLightPosition] = glGetUniformLocation(g_shader.program(), 
      "uLightPosition");

  for (uint32_t i = 0; i < kNumBlurTextures; ++i)
  {
    boost::format f = boost::format("blurMap%1%") % i;
    gBlurMaps[i] = glGetUniformLocation(gBlurShader.program(), f.str().c_str());
  }

}

void setupRC()
{
  glClearColor(0.15f, 0.15f, 0.15f,1.0f);
  if (!initContent() )
  {
    cout << "init content error\n";
    return;
  }
  setupUniforms();

  if (!initGeometry()) return;

  glEnable(GL_DEPTH_TEST);
  glEnable(GL_DITHER);

  gReady = true;
  if (checkGLErrors()) cout << "Setup RC" << endl;
}

void shutdownRC(void)
{
  g_shader.clean();
//  glBindFrameBuffer(GL_DRAW_FRAMEBUFFER, 0);
//  glBindFrameBuffer(GL_READ_FRAMEBUFFER, 0);
  for (int i = 0; i < kNumBlurTextures; ++i)
  {
    glActiveTexture(GL_TEXTURE0+i);
    glBindTexture(GL_TEXTURE_RECTANGLE, 0);
  }
  glDeleteTextures(kNumBlurTextures, gBlurTextures);
  glDeleteBuffers(1, pixBuffObjs);

  if (checkGLErrors()) cout << "Shutdown RC" << endl;
}

void AdvanceBlurTarget()
{
  gCurrentBlurTarget = (gCurrentBlurTarget + 1) % kNumBlurTextures;
}
static float gSpeed = 5.5f;

void drawGeometry(float t)
{
  glUseProgram(g_shader.program());
  glUniform1f(uniforms[kTime], t);
  LTransform MV = LTranslate(.0f, 0.0, -9.0) * LRotateX(LRadians(0)) * 
    LTranslate(3.2f * sin(gSpeed * t), 1.0f * cos(gSpeed * t), 0) *
    LRotateY(LRadians(30.0f * gSpeed * t));
  LTransform MVP = g_projection * MV;
  glUniformMatrix4fv(uniforms[kModelView], 1, GL_FALSE, MV.array());
  glUniformMatrix4fv(uniforms[kMVP], 1, GL_FALSE, MVP.array());

  LTransform normalTransform(MV.inverse().matrix().transpose());
  glUniformMatrix4fv(uniforms[kNormalMatrix], 1, GL_FALSE, normalTransform.array());

  LPoint lp(-5,5,5);
  glUniform3fv(uniforms[kLightPosition], 1, (float*)&lp);

  g_batches[g_selectedBatch].bind();
  g_batches[g_selectedBatch].draw();
  checkGLErrors();
}

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
    if (0)
    {
      glReadPixels(0, 0, gViewWidth, gViewHeight, GL_RGB, GL_UNSIGNED_BYTE, &gPixelData[0]);
      glActiveTexture(GL_TEXTURE0+GetBlurTarget(0));
      glBindTexture(GL_TEXTURE_RECTANGLE, gBlurTextures[GetBlurTarget(0)]);
      glTexImage2D(GL_TEXTURE_RECTANGLE, 0, GL_RGB8, gViewWidth, gViewHeight, 0, GL_RGB, 
          GL_UNSIGNED_BYTE, &gPixelData[0]);
    }
    else
    {
      glBindBuffer(GL_PIXEL_PACK_BUFFER, pixBuffObjs[0]);
      glReadPixels(0, 0, gViewWidth, gViewHeight, GL_RGB, GL_UNSIGNED_BYTE, NULL);
      glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);

      glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pixBuffObjs[0]);
      glActiveTexture(GL_TEXTURE0+GetBlurTarget(0));
      glBindTexture(GL_TEXTURE_RECTANGLE, gBlurTextures[GetBlurTarget(0)]);
      glTexImage2D(GL_TEXTURE_RECTANGLE, 0, GL_RGB8, gViewWidth, gViewHeight, 
          0, GL_RGB, GL_UNSIGNED_BYTE, NULL);
      glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    }

    glUseProgram(gBlurShader.program());
    LTransform MVP = gScreenProjection;
    glUniformMatrix4fv(uniforms[kBlurMVP], 1, GL_FALSE, MVP.array());

    for (uint32_t i = 0; i < kNumBlurTextures; ++i)
    {
      glUniform1i(gBlurMaps[i], GetBlurTarget(i));
      glActiveTexture(GL_TEXTURE0+GetBlurTarget(i));
      glBindTexture(GL_TEXTURE_RECTANGLE, gBlurTextures[GetBlurTarget(i)]);
    }
    glActiveTexture(GL_TEXTURE0);
    gScreenQuad.bind();
    gScreenQuad.draw();
    if (gShowFps) gSystemFont->write(0, 0, fpsStr);

    AdvanceBlurTarget();
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

  gSystemFont->setViewport(w, h);
  gViewWidth = w;
  gViewHeight = h;

  uint32_t pixelDataSize = w*h*3*sizeof(uint8_t);
  gPixelData.resize(pixelDataSize);

  glBindBuffer(GL_PIXEL_PACK_BUFFER, pixBuffObjs[0]);
  glBufferData(GL_PIXEL_PACK_BUFFER, pixelDataSize, &gPixelData[0], GL_DYNAMIC_COPY);
  glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);

  gScreenProjection = LOrtho(0, w, 0, h);
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
    gBlurShader.build();
    setupUniforms();
  }
  else if (key == 'f' || key == 'F')
  {
    gShowFps = !gShowFps;
  }
  else if (key == 'j' || key == 'J')
  {
    gSpeed = min(gSpeed + 0.5f, 10.0f);
    cout << "Speed=" << gSpeed << "\n";
  }
  else if (key == 'k' || key == 'K')
  {
    gSpeed = max(gSpeed - 0.5f, 0.1f);
    cout << "Speed=" << gSpeed << "\n";
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
    SaveScreen("motionblur");
  }
  else if (key == 'd' || key == 'D')
  {
    glutSwapBuffers();

    for (uint32_t i = 0; i < kNumBlurTextures; ++i)
    {
      glActiveTexture(GL_TEXTURE0 + GetBlurTarget(i));
      glBindTexture(GL_TEXTURE_RECTANGLE, gBlurTextures[GetBlurTarget(i)]);
      boost::format f = boost::format("blur%1%") % i;
      SaveTextureRect(f.str());
    }
  }
  glutPostRedisplay();
}

int main(int argc, char *argv[])
{
  gViewWidth = 1280;
  gViewHeight = 720;

  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowSize(gViewWidth, gViewHeight);
  glutCreateWindow("motionblur");
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




