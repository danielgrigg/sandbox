#ifndef SCENE_H
#define SCENE_H

#include <QGraphicsScene>
#include <QGLBuffer>
#include <QGLShaderProgram>
#include <QElapsedTimer>
#include <tr1/memory>
#include "TriBatch.h"
#include "TriMesh.h"
#include <QtGui>

using namespace std::tr1;

class Scene : public QGraphicsScene
{
    Q_OBJECT
public:
    explicit Scene(QObject *parent=NULL);

  QDialog *createDialog(const QString &windowTitle) const;
signals:

public slots:

protected:
  void drawBackground(QPainter *painter, const QRectF &);
private:

  bool createMesh();

  template<typename MeshType>
  void buildMesh(shared_ptr<MeshType> src);

  void render();

  QGLWidget* _viewport;
  QGLShaderProgram *program;

  //shared_ptr<TriMesh<lap::VertexP16N16> > _batch;
  shared_ptr<TriMesh<lap::VertexPNf> > _batch;
  QTimer *m_timer;
  QElapsedTimer _appTime;
  QLabel* _label;
};

#endif // OPENGLSCENE_H
