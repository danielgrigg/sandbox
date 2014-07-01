#include <QtGui/QApplication>
#include "window.h"
#include "graphicsview.h"
#include "scene.h"
#include <QWidget>
#include <QGLWidget>
#include "textfile.h"
#include <iostream>

int main(int argc, char *argv[])
{
  string programText =
      "90.0\n"
      "F-F-F-F\n"
      "F -> F-F+F+FF-F-F+F\n";
  programText =
      "90.0\n"
      "F+F+F+F\n"
      "F -> F+f-FF+F+FF+Ff+FF-f+FF-F-FF-Ff-FFF\n"
      "f -> ffffff\n";
  if (argc > 1)
  {
    if (!TextFileReader::read(argv[1], programText))
    {
      std::cerr << "Error reading l-file '" << argv[1] << "'." << std::endl;
      return 1;
    }
  }

  QApplication app(argc, argv);

  QGLWidget* widget = new QGLWidget(QGLFormat(QGL::SampleBuffers));
  widget->makeCurrent(); // The current context must be set before calling Scene's constructor
  GraphicsView view;
  view.setViewport(widget);
  view.setViewportUpdateMode(QGraphicsView::FullViewportUpdate);

  view.setScene(new Scene(programText));
  view.resize(720, 720);
  view.move(0, 0);
  view.show();
  return app.exec();
}
