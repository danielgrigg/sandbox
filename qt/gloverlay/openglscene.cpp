#include "openglscene.h"
#include <QtGui>
#include <QGLWidget>
#include <iostream>

int gVertexOffset = 0;

Scene::Scene(QObject *parent):
    QGraphicsScene(parent)
{
    QWidget *instructions = createDialog(tr("Instructions"));
    instructions->layout()->addWidget(new QLabel(tr("This is a dialog box!")));
    addWidget(instructions);

    QPointF pos(10, 10);
    foreach (QGraphicsItem *item, items()) {
        item->setFlag(QGraphicsItem::ItemIsMovable);
        item->setCacheMode(QGraphicsItem::DeviceCoordinateCache);

        const QRectF rect = item->boundingRect();
        item->setPos(pos.x() - rect.x(), pos.y() - rect.y());
        pos += QPointF(0, 10 + rect.height());
    }

    if (!QGLShaderProgram::hasOpenGLShaderPrograms())
    {
      qDebug() << "oh noes shaders\n";
    }
    qDebug() << "has shaders\n";
    QGLShader *fshader = new QGLShader(QGLShader::Fragment, this);
    const char *fsrc =
        "void main(void)\n"
        "{\n"
        "   gl_FragColor = vec4(1,0,0,1);\n"
        "}";

    if (!fshader->compileSourceCode(fsrc))
    {
      qDebug() << "fshader log:\n" << fshader->log();
    }

    QGLShader *vshader = new QGLShader(QGLShader::Vertex, this);
    const char *vsrc =
        "attribute highp vec4 vVertex;\n"
        "void main(void)\n"
        "{\n"
        "    gl_Position = vVertex;\n"
        "}\n";

    if (!vshader->compileSourceCode(vsrc))
    {
      qDebug() << vshader->log();
    }

    program = new QGLShaderProgram(this);
    program->addShader(vshader);
    program->addShader(fshader);
    //    program->bindAttributeLocation("vVertex", PROGRAM_VERTEX_ATTRIBUTE);
    if (!program->link())
    {
      qDebug() << program->log();
    }
    gVertexOffset =	program->attributeLocation("vVertex");

    //program->bind();

    vertexBuffer.create();
    if (!vertexBuffer.isCreated())
    {
      std::cout << "failed creating vertexBuffer" << std::endl;
    }
    std::cout << "created vbuffer" << std::endl;
    vertexBuffer.bind();
    float vs[] = {-1.0f, -1.0f, 0.0f,
                  1.0f, -1.0f, 0.0f,
                  0.0f, 1.0f, 0.0f};
    vertexBuffer.allocate(vs, 9 * sizeof(float));
}

QDialog *Scene::createDialog(const QString &windowTitle) const
{
  QDialog *dialog = new QDialog(0, Qt::CustomizeWindowHint | Qt::WindowTitleHint);

  dialog->setWindowOpacity(0.8);
  dialog->setWindowTitle(windowTitle);
  dialog->setLayout(new QVBoxLayout);

  return dialog;
}

void Scene::drawBackground(QPainter *painter, const QRectF &)
{
  //  QPaintEngine* pe = painter->paintEngine();
  //  qDebug("pe type: %d\n", pe->type());

  if (painter->paintEngine()->type() != QPaintEngine::OpenGL2)
  {
    qWarning("OpenGLScene: drawBackground needs a "
             "QGLWidget to be set as viewport on the "
             "graphics view");

    return;
  }
  painter->beginNativePainting();

  int width = painter->device()->width();
  int height = painter->device()->height();
  glViewport(0, 0, width, height);
  glClearColor(0.2, 0.2, 0.9, 1.0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();

//  glEnable(GL_MULTISAMPLE);
  program->bind();
//  program->setUniformValue("color", 1.0f, 0.0f, 0.0f, 1.0f);
  program->enableAttributeArray(gVertexOffset);
  vertexBuffer.bind();
  program->setAttributeArray(gVertexOffset, (float*)NULL, 3);
  glDrawArrays(GL_TRIANGLES, 0, 3);
  vertexBuffer.release();
  program->disableAttributeArray(gVertexOffset);
  program->release();

/*
  program->bind();
//  program->setUniformValue("color", 1.0f, 0.0f, 0.0f, 1.0f);
  float vs[] = {-1.0f, -1.0f, 0.0f,
                1.0f, -1.0f, 0.0f,
                0.0f, 1.0f, 0.0f};
  glVertexPointer(3, GL_FLOAT, 0, vs);
  program->enableAttributeArray(gVertexOffset);
  program->setAttributeArray(gVertexOffset, vs, 3);
  glDrawArrays(GL_TRIANGLES, 0, 3);
  program->disableAttributeArray(gVertexOffset);
  program->release();
*/
  /*
  float vs[] = {-1.0f, -1.0f, 0.0f,
                1.0f, -1.0f, 0.0f,
                0.0f, 1.0f, 0.0f};
  glVertexPointer(3, GL_FLOAT, 0, vs);
  glEnableClientState(GL_VERTEX_ARRAY);
  glDrawArrays(GL_TRIANGLES, 0, 3);
  glDisableClientState(GL_VERTEX_ARRAY);
*/

 // glDisable(GL_MULTISAMPLE);

  glPopMatrix();
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();

  painter->endNativePainting();
}
