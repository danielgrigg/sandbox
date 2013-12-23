#-------------------------------------------------
#
# Project created by QtCreator 2011-08-01T00:04:25
#
#-------------------------------------------------

QT       += core gui opengl

TARGET = objdraw
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp \
    glwidget.cpp \
    scene.cpp \
    graphicsview.cpp \
    tribatch.cpp \
    trimesh.cpp

HEADERS  += mainwindow.h \
    glwidget.h \
    scene.h \
    graphicsview.h \
    tribatch.h \
    trimesh.h

FORMS    += mainwindow.ui

LIBS += -L/usr/local/lib -llap
LIBS += -lboost_system -lboost_filesystem

INCLUDEPATH += /usr/local/include

CONFIG-=app_bundle
