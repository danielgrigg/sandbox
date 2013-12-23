#include <QtGui/QApplication>
#include <QDeclarativeView>
#include <QDeclarativeEngine>
#include <QDeclarativeComponent>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    QDeclarativeView* view = new
        QDeclarativeView(QUrl("qrc:/quickTutorial.qml"));
    view->show();

    return a.exec();
}
