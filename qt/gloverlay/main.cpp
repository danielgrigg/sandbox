#include <QtGui/QApplication>
#include "window.h"
#include "graphicsview.h"
#include "openglscene.h"
#include <QWidget>
#include <QGLWidget>
//#include "glwidget.h"

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
  view.resize(512, 512);
  return app.exec();
}
