#-------------------------------------------------
#
# Project created by QtCreator 2011-07-07T12:19:06
#
#-------------------------------------------------

QT       += core gui opengl

TARGET = helloIrr
TEMPLATE = app


SOURCES += main.cpp\
        irrwidget.cpp

HEADERS  += irrwidget.h

FORMS    += irrwidget.ui

INCLUDEPATH += /Users/daniel/tools/irrlicht-1.7.2/include

LIBS += -L/Users/daniel/tools/irrlicht-1.7.2/source/Irrlicht/MacOSX/build/Debug \
    -lIrrlicht

QMAKE_LFLAGS += -F /System/Library/Frameworks

LIBS += -framework Cocoa
LIBS += -framework IOKit
LIBS += -framework Carbon
