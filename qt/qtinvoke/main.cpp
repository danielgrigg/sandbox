#include <QtGui/QApplication>
#include "myclass.h"
#include <QDeclarativeEngine>
#include <QDeclarativeComponent>
#include <QDeclarativeView>
#include <QDeclarativeContext>

int main(int argc, char *argv[])
{
  QApplication a(argc, argv);

  QDeclarativeEngine engine;
  QDeclarativeComponent component(&engine, QUrl("qrc:/invoke.qml"));
  QObject *object = component.create();

  QVariant returnedValue;
  QVariant msg = "Hello from C++";
  QMetaObject::invokeMethod(object, "myQmlFunction",
                            Q_RETURN_ARG(QVariant, returnedValue),
                            Q_ARG(QVariant, msg));

  qDebug() << "QML function returned:" << returnedValue.toString();
  delete object;

  QDeclarativeView view;
  MyClass myClass;
  view.rootContext()->setContextProperty("myObject", &myClass);

  view.setSource(QUrl("qrc:/invoke.qml"));
  view.show();

  return a.exec();
}
