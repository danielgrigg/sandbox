#-------------------------------------------------
#
# Project created by QtCreator 2011-08-10T23:26:29
#
#-------------------------------------------------

QT       += core gui opengl

TARGET = doldraw
TEMPLATE = app


SOURCES += main.cpp\
        window.cpp \
    glwidget.cpp \
    graphicsview.cpp \
    scene.cpp \
    lineshape.cpp \
    dol.cpp \
    lineturtle.cpp \
    textfile.cpp

HEADERS  += window.h \
    glwidget.h \
    scene.h \
    graphicsview.h \
    lineshape.h \
    dol.h \
    lineturtle.h \
    common.h \
    textfile.h

FORMS    += window.ui

LIBS += -L/usr/local/lib -llap
LIBS += -lboost_system -lboost_filesystem -lboost_regex

INCLUDEPATH += /usr/local/include

CONFIG-=app_bundle
