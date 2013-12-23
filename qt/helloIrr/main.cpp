#include <QtGui/QApplication>
#include "irrwidget.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    IrrWidget *widget = new IrrWidget(QString( "Qt/Irrlicht" ));
    widget->setVisible( true );

    return a.exec();
}
