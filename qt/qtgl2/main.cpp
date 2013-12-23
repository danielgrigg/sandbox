#include <QtGui/QApplication>
#include "window.h"
#include "glwidget.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    Window w;
    w.show();

    GLWidget* canvas = new GLWidget(NULL);
    //  canvas->resize(512,512);
    canvas->show();
    return a.exec();
}
