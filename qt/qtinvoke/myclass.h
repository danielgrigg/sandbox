#ifndef MYCLASS_H
#define MYCLASS_H

#include <QObject>
#include <QString>
#include <QDebug>

class MyClass : public QObject
{
  Q_OBJECT
public:
  Q_INVOKABLE void cppMethod(const QString &msg) {
    qDebug() << "Called the C++ method with" << msg;
  }

public slots:
  void cppSlot(int number) {
    qDebug() << "Called the C++ slot with" << number;
  }
};

#endif // MYCLASS_H
