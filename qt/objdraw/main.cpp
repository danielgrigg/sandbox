#include <QtGui/QApplication>
#include "mainwindow.h"
#include "graphicsview.h"
#include "scene.h"
#include <QWidget>
#include <QGLWidget>

int main(int argc, char *argv[])
{
  QApplication app(argc, argv);


  QGLWidget* widget = new QGLWidget(QGLFormat(QGL::SampleBuffers));
  widget->makeCurrent(); // The current context must be set before calling Scene's constructor
  GraphicsView view;
  view.setViewport(widget);
  view.setViewportUpdateMode(QGraphicsView::FullViewportUpdate);

  view.setScene(new Scene());
  view.show();
  view.resize(1024, 1024);
  return app.exec();
}
