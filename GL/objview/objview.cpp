/*
 *  \file objview.cpp
 *  \brief objview.cpp
 *  \author Daniel Grigg 
 *  \date 10/10/10.
 *  objview
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
#include "../shared/ObjModel.h"
#include "../shared/LFont.hpp"
#include "../shared/MeshCompute.h"
#include <boost/lexical_cast.hpp>
#include <lexi/Geometry/LAABB.h>
#include <boost/bind.hpp>

using namespace std;
using namespace lmc;

const std::string mediaPath = "../Resources";

enum 
{
  kModelView,
  kMVP,
  kNormalMatrix,
  kTime,
  kLightPosition,
  kUniformMax
};

struct CameraSettings
{
  float fov;
  uint32_t viewWidth;
  uint32_t viewHeight;
  float nearPlane;
  float farPlane;
  float aspect()const { return (float)viewWidth / (float)viewHeight; }
  LTransform projection()const 
  { 
    return LPerspective(LRadians(fov), aspect(), nearPlane, farPlane);
  }
};
//CameraSettings gCamera = {38.0f, 512, 512, 1.0f, 100.0f};
CameraSettings gCamera = {38.0f, 1280, 720, 2.0f, 500.0f};

GLuint uniforms[kUniformMax];
bool gReady = false;
LTransform g_projection;
LShader g_shader;
static bool g_paused = false;
static LStopwatch g_appTimer;
static vector<string> gModelFiles;
static LAABB gSceneBounds;
static uint32_t gDesiredMg = 0;
static float gRotateX = 0.0f;
static float gRotateY = 0.0f;

void makeGeometryBatch(const Mesh& mesh, GeometryBatch& geomBatch, const string& name)
{
  geomBatch.init(GL_TRIANGLES, name, GeometryBatch::kStatic);
  geomBatch.ps.resize(mesh.vs.size());
  geomBatch.ns.resize(mesh.vs.size());

  for (int i = 0; i < mesh.vs.size(); ++i)
  {
    const vec3f& p = mesh.vs[i];
    const vec3f& n = mesh.ns[i];
    geomBatch.ps[i] = LPoint(p.x, p.y, p.z);
    geomBatch.ns[i] = LNormal(n.x, n.y, n.z);
  }
  geomBatch.elements.resize(mesh.is.size());
  std::copy(mesh.is.begin(), mesh.is.end(), geomBatch.elements.begin());
  geomBatch.boundingBox = LAABB(&geomBatch.ps[0], geomBatch.ps.size());
}

struct Shape
{
  GeometryBatch m_batch;
  ObjMaterialMap m_materials; 
  std::vector<MeshGroup> m_groups;

  Shape()
  {
  }

  bool importObjFile(const string& filename)
  {
    Mesh mesh;
    if (!mesh.importFromObjFile(filename)) 
    { 
      std::cerr << "failed importing '" << filename << "'\n";
      return false;
    }
    Mesh alignedMesh;
    mesh.mergeMaterialGroups(alignedMesh);
    std::cout << "AlignedMesh " << filename << ", " 
      << alignedMesh.vs.size() << " vertices, " 
      << alignedMesh.is.size() /3 << " triangles, "
      << alignedMesh.is.size() << " indices\n";

    makeGeometryBatch(alignedMesh, m_batch, filename);
    m_materials = alignedMesh.m_materials;
    m_groups = alignedMesh.m_groups;

    Kd_loc = glGetUniformLocation(g_shader.program(), "Kd");
    Ka_loc = glGetUniformLocation(g_shader.program(), "Ka");
    Ns_loc = glGetUniformLocation(g_shader.program(), "Ns");
    Ks_loc = glGetUniformLocation(g_shader.program(), "Ks");

    return true;
  }

  bool importMesh(const Mesh& mesh, const std::string& name)
  {
    makeGeometryBatch(mesh, m_batch, name);
    m_materials = mesh.m_materials;
    m_groups = mesh.m_groups;

    Kd_loc = glGetUniformLocation(g_shader.program(), "Kd");
    Ka_loc = glGetUniformLocation(g_shader.program(), "Ka");
    Ns_loc = glGetUniformLocation(g_shader.program(), "Ns");
    Ks_loc = glGetUniformLocation(g_shader.program(), "Ks");
    return true;
  }

  LAABB boundingBox()const { return m_batch.boundingBox; }

  void drawMaterialGroup(const MeshMaterialGroup& mg)
  {
//    cout << "drawing material-group " << mg << endl;
    ObjMaterial& material = m_materials[mg.m_material];
    glUniform3fv(Kd_loc, 1, material.Kd);
    glUniform3fv(Ka_loc, 1, material.Ka);
    glUniform1f(Ns_loc, material.Ns);
    glUniform3fv(Ks_loc, 1, material.Ks);

    m_batch.drawRange(mg.m_start, mg.m_count, mg.m_minElement, mg.m_maxElement);
  }

  void draw()
  {
    static int calls = 0;
//    if (calls++ > 0) return;

    uint32_t mgIdx = 0;
    m_batch.bind();
    for (std::vector<MeshGroup>::const_iterator groupIt = m_groups.begin();
        groupIt != m_groups.end(); ++groupIt)
    {
//      cout << "drawing group " << groupIt->m_name << ", start " << 
//        groupIt->m_start << ", " << groupIt->m_count << "\n";
      for (std::vector<MeshMaterialGroup>::const_iterator mgIt = 
          groupIt->m_materialGroups.begin(); 
          mgIt != groupIt->m_materialGroups.end();
          ++mgIt)
      {
//        if (mgIdx == gDesiredMg)
        {
          drawMaterialGroup(*mgIt);
        }
        mgIdx++;
      }
    }
  }
  private:
  GLuint Kd_loc;
  GLuint Ka_loc;
  GLuint Ns_loc;
  GLuint Ks_loc;
};

bool makeShapeChunks(const std::string& objFile, vector<Shape>& outShapes)
{
  Mesh mesh;
  if (!mesh.importFromObjFile(objFile)) 
  { 
    std::cerr << "failed importing '" << objFile << "'\n";
    return false;
  }
  Mesh alignedMesh;
  mesh.mergeMaterialGroups(alignedMesh);

  std::cout << "AlignedMesh " << objFile << ", " 
    << alignedMesh.vs.size() << " vertices, " 
    << alignedMesh.is.size() /3 << " triangles, "
    << alignedMesh.is.size() << " indices\n";

  vector<Mesh> meshChunks;
  alignedMesh.partition(meshChunks, 300000);
  std::cout << "Partitioned into " << meshChunks.size() << " chunks.\n";

  for (uint32_t i = 0; i < meshChunks.size(); ++i)
  {
    outShapes.push_back(Shape());
    std::string chunkId = boost::lexical_cast<string>(i);
    outShapes.back().importMesh(meshChunks[i], objFile + chunkId);
  }
  return true;
}

static vector<Shape> gShapes;
typedef std::tr1::shared_ptr<LFont> LFontPtr;
LFontPtr gSystemFont;

bool initShaders()
{
  vector<string> sources;
  sources.push_back("objview.fp");
  sources.push_back("objview.vp");

  vector<std::pair<string,int> > attributes;
  attributes.push_back(std::make_pair("vVertexPos", (int)GeometryBatch::kPosOffset));
  attributes.push_back(std::make_pair("vNormal", (int)GeometryBatch::kNormalOffset));
  g_shader.setSources(sources, attributes);
  bool result = g_shader.build();
  cout << "Shader: " << g_shader << endl;
  if (checkGLErrors()) cout << "Initialised shaders" << endl;
  return result;
}

bool initContent()
{
  gSystemFont = LFontPtr(new LFont("Consolas", 18, 96, "/Library/Fonts/Microsoft/"));
  if (!checkGLErrors()) return false;

  if (!initShaders() )
  {
    cout << "init shaders error\n";
    return false;
  }
  return true;
}

bool initGeometry()
{
  if (!checkGLErrors()) return false;;
  int n = 0;
  LStopwatch timer;

  gShapes.resize(gModelFiles.size());
  for (int i = 0; i < gModelFiles.size(); ++i)
  {
    cout << "Importing " << gModelFiles[i] << endl;
    //if (!gShapes[i].importObjFile(gModelFiles[i])) return false;
    if (!makeShapeChunks(gModelFiles[i], gShapes)) return false;
  }
  for (int i = 0; i < gShapes.size(); ++i)
  {
    gSceneBounds.unionWith(gShapes[i].boundingBox()); 
  }
  cout << "sceneBounds: " << gSceneBounds << endl;

  double dt = timer.timeElapsed_s();
  std::cout << "Imported models in " << dt << "s\n";
  if (checkGLErrors()) cout << "Initialised geometry" << endl;
  return true;
}

void drawGeometry()
{
  float t = g_appTimer.timeElapsed_s();
  glUseProgram(g_shader.program());
  glUniform1f(uniforms[kTime], t);

  float x = gSceneBounds.centre().x();
  float y = gSceneBounds.centre().y();
  float z = gSceneBounds.centre().z();
  float zX = .5f * gSceneBounds.xSize() / (gCamera.aspect() * tanf(LRadians(.5f * gCamera.fov)));
  float zY = .5f * gSceneBounds.ySize() / tanf(LRadians(.5f * gCamera.fov));
  float zMax = std::max(zX, zY) + .5f * gSceneBounds.zSize();
  LVector modelT(-x, -y , -zMax);

  LTransform centreTransform = LTranslate(0, 0, -1 - zMax) * 
//    LRotateY(LRadians(5.0f * t)) * LRotateX(LRadians(1.0f)) * 
    LRotateY(LRadians(gRotateY)) * LRotateX(LRadians(gRotateX)) *
    LTranslate(-x, -y, -z);

  //  LTransform M = LTranslate(10.0 * sinf(.2*t), -20, -60);
  LTransform M = centreTransform;
  //  LTransform M = LTranslate(modelT.x(), modelT.y(), modelT.z()) * // 10, -120
  //
  //LTransform M = LTranslate(-50.0f, -50, -155.0) * // 10, -120
  //    LRotateX(LRadians(20)) * 
  //    LRotateY(LRadians(0.0f * t)) * 
  //    LRotateX(LRadians(0));
  LTransform MVP = g_projection * M;
  LTransform normalTransform(M.inverse().matrix().transpose());

  glUniformMatrix4fv(uniforms[kModelView], 1, GL_FALSE, M.array());
  glUniformMatrix4fv(uniforms[kMVP], 1, GL_FALSE, MVP.array());
  glUniformMatrix4fv(uniforms[kNormalMatrix], 1, GL_FALSE, normalTransform.array());

  LPoint lp(9.0f*sinf(.9f * t),9.0 + 3.0f * sinf(.8 * t),15);
  glUniform3fv(uniforms[kLightPosition], 1, (float*)&lp);

  std::for_each(gShapes.begin(), gShapes.end(), boost::bind(&Shape::draw, _1));
  checkGLErrors();
}

void setupRC()
{
  glClearColor(0.25f, 0.25f, 0.25f,1.0f);

  if (!initContent() )
  {
    cout << "init content error\n";
    return;
  }


  uniforms[kModelView] = glGetUniformLocation(g_shader.program(), "mvMatrix");
  uniforms[kMVP] = glGetUniformLocation(g_shader.program(), "mvpMatrix");
  uniforms[kNormalMatrix] = 
    glGetUniformLocation(g_shader.program(), "normalMatrix");

  uniforms[kLightPosition] = glGetUniformLocation(g_shader.program(), 
      "uLightPosition");
  uniforms[kTime] = glGetUniformLocation(g_shader.program(), "uTime");
  if (!initGeometry()) return;

  glEnable(GL_DEPTH_TEST);
  glEnable(GL_DITHER);

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
  static uint64_t frameCounter = 0;
  static std::string fpsStr = "0";

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  if (gReady)
  {
    uint64_t t = g_appTimer.elapsedMillis();
    drawGeometry();
    if (frameCounter % 100 == 99)
    {
      uint32_t fps = 1000 * frameCounter / t;
      fpsStr = boost::lexical_cast<std::string>(fps);
      //fpsStr = boost::lexical_cast<std::string>((float)frameCounter / t);
    }
    gSystemFont->write(0, 0, fpsStr);
  }
  frameCounter += 1;
  glutSwapBuffers();
  glutPostRedisplay();
}

void ChangeSize(int w, int h)
{
  if(h == 0) h = 1;

  // Set Viewport to window dimensions
  glViewport(0, 0, w, h);

  gCamera.viewWidth = w;
  gCamera.viewHeight = h;
  g_projection = gCamera.projection();
  gSystemFont->setViewport(w, h);
}

void KeyPressFunc(unsigned char key, int x, int y)
{
  const float dAngle = 15.0f;
  if(key == 'n' || key == 'N' || key == 0x20)
  {
    gDesiredMg = (gDesiredMg + 1) % gShapes[0].m_groups[0].m_materialGroups.size();
  }
  else if (key == 'b' || key == 'B')
  {
    g_shader.build();
  }
  else if (key == 'h' || key == 'H')
  {
    gRotateY -= dAngle;
  }
  else if (key == 'l' || key == 'L')
  {
    gRotateY += dAngle;
  }
  else if (key == 'j' || key == 'J')
  {
    gRotateX -= dAngle;
  }
  else if (key == 'k' || key == 'K')
  {
    gRotateX += dAngle;
  }
  else if (key == 'm' || key == 'M')
  {
    static bool drawLines = false;
    drawLines = !drawLines;
    if (drawLines) glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    else glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
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
    SaveScreen("objview");
  }
  glutPostRedisplay();
}


int main(int argc, char *argv[])
{
  bool gotParams = true;
  if (argc < 2)
  {
    cerr << "Usage: objview <obj-file> ..." << endl;
    gotParams = false;
  }
  else
  {
    for (int i = 1; i < argc ; ++i)
    {
      gModelFiles.push_back(argv[i]);
    }
  }
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH);
  glutInitWindowSize(gCamera.viewWidth, gCamera.viewHeight);
  glutCreateWindow("objview");
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
  if (gReady && gotParams) glutMainLoop();
  shutdownRC();
  return 0;
}

