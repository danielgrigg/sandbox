#ifndef MYGLDRAWER_H
#define MYGLDRAWER_H

#include <QtOpenGL/QGLWidget>

class MyGLDrawer : public QGLWidget {

  Q_OBJECT // must include this if you use Qt signals/slots

public:
  MyGLDrawer(QWidget *parent = NULL);

protected:
  void initializeGL();
  void resizeGL(int w, int h);
  void paintGL();
  void mousePressEvent(QMouseEvent *event);
  void mouseMoveEvent(QMouseEvent *event);
  void keyPressEvent(QKeyEvent *event);
};


#endif // MYGLDRAWER_H
