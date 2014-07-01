#include "scene.h"
#include <QtGui>
#include <QGLWidget>
#include <iostream>
#include <tr1/memory>
#include <lap/lap.h>
#include <string>
#include <iostream>
#include <boost/lexical_cast.hpp>
#include <cstddef>
#include "common.h"

using namespace lap;
using namespace std;
using namespace std::tr1;

int gMVP = 0;

Scene::Scene(const std::string& programText, QObject *parent):
    QGraphicsScene(parent)
{
    QWidget *instructions = createDialog(tr("Instructions"));
    _label = new QLabel(tr("This is a dialog box!"));
    _buttonStep = new QPushButton(tr("Step"));
    _buttonReset = new QPushButton(tr("Reset"));
//    _buttonStep->setWindowOpacity(0.5);
    addWidget(_buttonStep);
    addWidget(_buttonReset);

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
    QGLShader *fshader = new QGLShader(QGLShader::Fragment, this);
    const char *fsrc =
        "uniform float time;"
        "void main(void)\n"
        "{\n"
        "gl_FragColor = vec4(1,1,0,1);\n"
//        "   gl_FragColor = vec4(.5 + .5 * sin(.99 * time),\n"
//        "                       .5 + .5 * sin(1.7 * (time+.4)),\n"
//        "                       .5 + .5 * sin(1.3 * (time+.9)),\n"
//        "                       1);\n"
        "}";

    if (!fshader->compileSourceCode(fsrc))
    {
      qDebug() << "fshader log:\n" << fshader->log();
    }

    QGLShader *vshader = new QGLShader(QGLShader::Vertex, this);
    const char *vsrc =
        "uniform mat4 mvpMatrix;"
        "attribute highp vec4 vPosition;\n"
        "void main(void)\n"
        "{\n"
        "    gl_Position = mvpMatrix * vPosition;\n"
        "}\n";

    if (!vshader->compileSourceCode(vsrc))
    {
      qDebug() << "vshader log:\n" << vshader->log();
    }

    program = new QGLShaderProgram(this);
    program->addShader(vshader);
    program->addShader(fshader);
    program->bindAttributeLocation(kAttributes[kPosition], kPosition);
    if (!program->link())
    {
      qDebug() << program->log();
    }
    gMVP =	program->uniformLocation("mvpMatrix");

    _turtle = LineTurtle::makeLineTurtle(programText, 1.0f);
    for (int i = 0; i < 2; ++i) _turtle->step();

    connect(_buttonStep, SIGNAL(clicked()), this, SLOT(step()));
    connect(_buttonReset, SIGNAL(clicked()), this, SLOT(reset()));

    _appTime.start();
    m_timer = new QTimer(this);
    m_timer->setInterval(20);
    connect(m_timer, SIGNAL(timeout()), this, SLOT(update()));
    m_timer->start();
}

void Scene::step()
{
  _turtle->step();
}

void Scene::reset()
{
  _turtle->reset();
}

QDialog *Scene::createDialog(const QString &windowTitle) const
{
  QDialog *dialog = new QDialog(0, Qt::CustomizeWindowHint | Qt::WindowTitleHint);

  dialog->setWindowOpacity(0.5);
  dialog->setWindowTitle(windowTitle);
  dialog->setLayout(new QVBoxLayout);

  return dialog;
}

void Scene::render()
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glLineWidth(2.0);

  program->bind();
  QMatrix4x4 m;
  std::string labelText = boost::lexical_cast<string>(_appTime.elapsed() / 1000);
  _label->setText(QString::fromStdString(labelText));
  QMatrix4x4 mvp;
  lap::float3 centre = _turtle->bounds().centre();
  const lap::BoundingBox<lap::float3>& B = _turtle->bounds();
  float s = B.size(B.largestAxis());

  mvp.ortho(B.min()[0], B.min()[0]+s, B.min()[1], B.min()[1]+s, -1, 1);
//  mvp.translate(-centre[0], -centre[1], 0);
  program->setUniformValue("mvpMatrix", mvp);
  program->setUniformValue("time", _appTime.elapsed() / 1000.0f);
  _turtle->render(program);
  program->release();

}

void Scene::drawBackground(QPainter *painter, const QRectF &)
{
  painter->beginNativePainting();

  int width = painter->device()->width();
  int height = painter->device()->height();
  glViewport(0, 0, width, height);
  glClearColor(0.2, 0.2, 0.2, 1.0);
  glEnable(GL_DEPTH_TEST);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();

  glEnable(GL_MULTISAMPLE);

  render();

  QGLBuffer::release(QGLBuffer::VertexBuffer);

  glDisable(GL_DEPTH_TEST);
  glDisable(GL_MULTISAMPLE);

  glPopMatrix();
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();

  painter->endNativePainting();
}

void Scene::keyPressEvent(QKeyEvent *event)
{
  if (event->key() == Qt::Key_Space)
  {
    step();
  }
}
