#ifndef SCENE_H
#define SCENE_H

#include <QGraphicsScene>
#include <QGLBuffer>
#include <QGLShaderProgram>
#include <QElapsedTimer>
#include <tr1/memory>
#include "LineShape.h"
#include <QtGui>
#include "LineTurtle.h"

using namespace std::tr1;

class Scene : public QGraphicsScene
{
    Q_OBJECT
public:
    explicit Scene(const std::string& programText, QObject *parent=NULL);

  QDialog *createDialog(const QString &windowTitle) const;
signals:

public slots:
  void render();
  void step();
  void reset();

protected:
  void drawBackground(QPainter *painter, const QRectF &);
  void keyPressEvent(QKeyEvent *event);
private:


  template<typename MeshType>
  void buildMesh(shared_ptr<MeshType> src);

  QGLWidget* _viewport;
  QGLShaderProgram *program;
  QTimer *m_timer;
  QElapsedTimer _appTime;
  QLabel* _label;
  QPushButton* _buttonStep;
  QPushButton* _buttonReset;

  LineTurtlePtr _turtle;
};

#endif // OPENGLSCENE_H
