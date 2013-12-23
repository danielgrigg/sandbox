#include <QtGui/QApplication>
#include "mainwindow.h"
#include "mygldrawer.h"

int main(int argc, char *argv[])
{
  QApplication a(argc, argv);
  MyGLDrawer* painter = new MyGLDrawer(NULL);
  painter->resize(512,512);
  painter->show();
  return a.exec();
}
