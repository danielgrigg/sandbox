#include <QtGui>
#include <QtOpenGL>
#include "glwidget.h"
#include <iostream>

#define PROGRAM_VERTEX_ATTRIBUTE 0

GLWidget::GLWidget(QWidget *parent, QGLWidget *shareWidget)
    : QGLWidget(parent, shareWidget)
{
}

GLWidget::~GLWidget()
{
}

void GLWidget::initializeGL()
{
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
}

void GLWidget::paintGL()
{
  makeCurrent();
//  qglClearColor(QColor(0,0,0));
  glClearColor(.2, .2, .5, 1.0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

void GLWidget::resizeGL(int width, int height)
{
  glViewport(0, 0, width, height);
}
