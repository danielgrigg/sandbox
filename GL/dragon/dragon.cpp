/*
 *  \file TEMPLATE.cpp
 *  \brief TEMPLATE.cpp
 *  \author Daniel Grigg 
 *  \date 10/10/10.
 *  Tunnel
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

const std::string mediaPath = "../Resources";

enum
{
  kPosOffset,
  kColorOffset
};

const char * kAttribs[2] = {
  "vVertexPos", "vColor"
};

LShader g_shader;


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
GLuint g_g_vertexArrayObject;
bool gReady = false;
LTransform g_projection;

float randomOne()
{
  return (float)rand() / (float)RAND_MAX;
}

// Expand a value (0, 1) -> (-1, +1)
float expand(float x)
{
  return 2.0f * (x - .5f);
}

// Shrink a value (-1, +1) -> (0, 1)
float shrink(float y)
{
  return .5f * (1.0f + y);
}

LPoint perturb(LPoint p)
{
  return p + LPoint(.1f * expand(randomOne()), 
                    .1f * expand(randomOne()),
                    .1f * expand(randomOne()));
}


class Fractal
{
  public:
    Fractal(const vector<LPoint> &seed, const vector<LColor> &colors):
      m_geometry(seed),
      m_colors(colors)
  {}
    virtual void step() = 0;

    const vector<LPoint> & geometry()const { return m_geometry; }
    const vector<LColor> & colors()const { return m_colors; }
  protected:
    vector<LPoint> m_geometry;
    vector<LColor> m_colors;
  private:
};

class Noisy : public Fractal
{
  public:
    Noisy(const vector<LPoint> &seed, const vector<LColor> &colors):
      Fractal(seed, colors)
  {
    m_seedPoints = geometry();
    m_seedColors = this->colors();
  }
    virtual void step()
    {
      LAssert(m_seedPoints.size() == geometry().size());
      std::transform(m_seedPoints.begin(), m_seedPoints.end(), 
          m_geometry.begin(), perturb);
  
      std::ostream_iterator<LPoint> out_it(cout, ", ");
      std::copy(m_geometry.begin(), m_geometry.end(), out_it);
      cout << endl;
      
    }
  private:
    vector<LPoint> m_seedPoints;
    vector<LColor> m_seedColors;
};

class Identity : public Fractal
{
  public:
    Identity(const vector<LPoint> &seed, const vector<LColor> &colors):
      Fractal(seed, colors)
  {}
    virtual void step()
    {
    }
};

class Dragon : public Fractal
{
  public:

  Dragon(const vector<LPoint> &seed, const vector<LColor> &colors):
      Fractal(seed, colors)
  {}
  void step()
  {
    if (m_geometry.size() > 1100000)
    {
      cout << "Number of points is getting waaay excessive, stop it!" << endl;
      return;
    }

    // For each successive line segment pair, insert two points. We refer to 
    // these two segments as their encompassing triangle, defined by points
    // A, B and C. The inserted points are U and V.
    // Point U is the mid-point of A and C. For simplicity, compute U as the 
    // projection of AB on AC.
    // Point V is then B + AU.
    int roundedSize = 2 * (m_geometry.size() / 2);

    // Updating is inefficient but the point is demonstration only.
    vector<LPoint> newGeometry;
    newGeometry.reserve(5 * (m_geometry.size() / 2) + 2);
    for (int i = 0; i < roundedSize; i+=2)
    {
      LPoint A = m_geometry[i];
      LPoint B = m_geometry[i+1];
      LPoint C = m_geometry[i+2];
      LVector AB = B - A; 
      LVector AC = normalize(C - A);
      float l = dot(AB, AC);
      LPoint U = A + l * AC;
      LPoint V = B + l * AC;

      newGeometry.push_back(A);
      newGeometry.push_back(U);
      newGeometry.push_back(B);
      newGeometry.push_back(V);
    }
    newGeometry.push_back(m_geometry.back());
    m_geometry = newGeometry;

    vector<LColor> newColors;
    newColors.reserve(5 * (m_colors.size() / 2) + 2);
    for (int i = 0; i < roundedSize; i+=2)
    {
      LColor A = m_colors[i];
      LColor B = m_colors[i+1];
      LColor C = m_colors[i+2];
      LColor U = A * 0.5f + B * 0.5f;
      LColor V = B * 0.5f + C * 0.5f;
      newColors.push_back(A);
      newColors.push_back(U);
      newColors.push_back(B);
      newColors.push_back(V);
    }
    newColors.push_back(m_colors.back());
    m_colors = newColors;
  }
};

tr1::shared_ptr<Fractal> g_fractal;

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

void upload(const vector<LPoint> &points, const vector<LColor> &colors)
{
  if (!noErrors()) return;
	glBindVertexArray(g_g_vertexArrayObject);
  glBindBuffer(GL_ARRAY_BUFFER, g_vertexArray);
  glBufferData(GL_ARRAY_BUFFER, sizeof(LPoint) * points.size(), &points[0], GL_DYNAMIC_DRAW);
  cout << "Uploaded " << points.size() << " points.\n";
  
  glBindBuffer(GL_ARRAY_BUFFER, g_colorArray);
  glBufferData(GL_ARRAY_BUFFER, sizeof(LColor) * colors.size(), &colors[0], GL_DYNAMIC_DRAW);
  
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
  sources.push_back("FlatShader.fp");
  sources.push_back("FlatShader.vp");

  vector<string> attributes;
  attributes.push_back("vVertexPos");
  attributes.push_back("vColor");
  g_shader.setSources(sources, attributes);
  bool result = g_shader.build();
  cout << "Shader: " << g_shader << endl;
  return result;
}

void KeyPressFunc(unsigned char key, int x, int y)
{
	if(key == 'n' || key == 'N' || key == 0x20)
  {
    g_fractal->step();
    upload(g_fractal->geometry(), g_fractal->colors());
  }
  else if (key == 'b' || key == 'B')
  {
    //initShaders();
    g_shader.build();
  }

  // Refresh the Window
  glutPostRedisplay();
}

void initGeometry()
{
  if (!noErrors()) return;
  glGenVertexArrays(1, &g_g_vertexArrayObject);
  glBindVertexArray(g_g_vertexArrayObject);
  glGenBuffers(1, &g_vertexArray);
  glGenBuffers(1, &g_colorArray);
  noErrors();
}

void drawCurve()
{
  static float t = 0.0;
  t += 0.01f;
  glUseProgram(g_shader.program());
  glBindVertexArray(g_g_vertexArrayObject);

  glUniform1f(uniforms[kTime], t);
  LTransform M = LTranslate(.0f, 0, -3.0f + sin(1.0f * t) ) * LRotateZ(10.0f * LRadians(t));
  LTransform MVP = g_projection * M;
  glUniformMatrix4fv(uniforms[kModelView], 1, GL_FALSE, M.asMatrix().asArray());
  glUniformMatrix4fv(uniforms[kMVP], 1, GL_FALSE, MVP.asMatrix().asArray());
  glDrawArrays(GL_LINE_STRIP, 0, g_fractal->geometry().size());
}

void drawBatch()
{
  glUseProgram(g_shader.program());
  glBindVertexArray(g_g_vertexArrayObject);

  LTransform T = LTranslate(0, 0, -2);
  glUniformMatrix4fv(uniforms[kModelView], 1, GL_FALSE, T.asMatrix().asArray());
  glDrawArrays(GL_TRIANGLES, 0, 3);
}


//////////////////////////////////////////////////////////////////
// This function does any needed initialization on the rendering
// context.  Here it sets up and initializes the texture objects.
void SetupRC()
{
  // Black background
  glClearColor(0.0f, 0.0f, 0.0f,1.0f);

  // init shaders
  if (! initShaders() )
  {
    cout << "init shaders error\n";
    return;
  }

  uniforms[kModelView] = glGetUniformLocation(g_shader.program(), "mvMatrix");
  uniforms[kMVP] = glGetUniformLocation(g_shader.program(), "mvpMatrix");
  uniforms[kTime] = glGetUniformLocation(g_shader.program(), "uTime");
  initGeometry();

  vector<LPoint> points;
  if (1) 
  {
    points.push_back(LPoint(0.5, 0.5, 0.0));
    points.push_back(LPoint(0.0, 0.0, 0.0));
    points.push_back(LPoint(-0.5, 0.5, 0.0));
  }
  else
  {
    points.push_back(LPoint(-0.8, 0.8, 0.0));
    points.push_back(LPoint(0.0, -0.8, 0.0));
    points.push_back(LPoint(0.8, 0.8, 0.0));
  }
  vector<LColor> colors;
  colors.push_back(LColor(1.0, 0.0, 0.0, 1.0));
  colors.push_back(LColor(0.0, 1.0, 0.0, 1.0));
  colors.push_back(LColor(0.0, 0.0, 1.0, 1.0));

  g_fractal = tr1::shared_ptr<Fractal>(new Dragon(points, colors));
  upload(g_fractal->geometry(), g_fractal->colors());
  gReady = true;

}

///////////////////////////////////////////////////
// Shutdown the rendering context. Just deletes the
// texture objects
void ShutdownRC(void)
{
  //  glDeleteTextures(TEXTURE_COUNT, textures);
  g_shader.clean();
}

/////////////////////////////////////////////////////////////////////
// Change viewing volume and viewport.  Called when window is resized
void ChangeSize(int w, int h)
{
  GLfloat fAspect;

  // Prevent a divide by zero
  if(h == 0)
    h = 1;

  // Set Viewport to window dimensions
  glViewport(0, 0, w, h);

  fAspect = (GLfloat)w/(GLfloat)h;
  g_projection = LPerspective(LRadians(40.0f), fAspect, 1.0f, 100.0f);
  //  glUniformMatrix4fv(uniforms[kProjection], 1, GL_FALSE, g_projection.asMatrix().asArray());

  // Produce the perspective projection
  //  viewFrustum.SetPerspective(80.0f,fAspect,1.0,120.0);
  //  transformPipeline.SetMatrixStacks(modelViewMatrix, projectionMatrix);
}

///////////////////////////////////////////////////////
// Called to draw scene
void RenderScene(void)
{
  // Clear the window with current clearing color
  glClear(GL_COLOR_BUFFER_BIT);

  if (gReady)
  {
    drawCurve();

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
  // Buffer swap
  glutSwapBuffers();

  glutPostRedisplay();
}


//////////////////////////////////////////////////////
// Program entry point

int main(int argc, char *argv[])
{
  // Standard initialization stuff
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
  glutInitWindowSize(1280, 720);
  glutCreateWindow("TEMPLATE");
  glutReshapeFunc(ChangeSize);
  glutDisplayFunc(RenderScene);
  glutKeyboardFunc(KeyPressFunc);

  // Add menu entries to change filter

  GLenum err = glewInit();
  if (GLEW_OK != err) 
  {
    fprintf(stderr, "GLEW Error: %s\n", glewGetErrorString(err));
    return 1;
  }

  //  LTransform P = LPerspective(LRadians(45.0f), 1.0f, 1, 100);

  // Startup, loop, shutdown
  SetupRC();
  glutMainLoop();
  ShutdownRC();

  return 0;
}




