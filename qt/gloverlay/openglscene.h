#ifndef OPENGLSCENE_H
#define OPENGLSCENE_H

#include <QGraphicsScene>
#include <QGLBuffer>
#include <QGLShaderProgram>

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
  QGLWidget* _viewport;
  QGLShaderProgram *program;
  QGLBuffer vertexBuffer;
};

#endif // OPENGLSCENE_H
