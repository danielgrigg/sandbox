#include <QtGui>
#include <QtOpenGL>
#include "glwidget.h"
#include <iostream>

#define PROGRAM_VERTEX_ATTRIBUTE 0

GLWidget::GLWidget(QWidget *parent, QGLWidget *shareWidget)
    : QGLWidget(parent, shareWidget)
{
}

GLWidget::~GLWidget()
{
}

void GLWidget::initializeGL()
{
//    glClearColor(0.5, 0.5, 0.5, 1.0);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    QGLShader *fshader = new QGLShader(QGLShader::Fragment, this);
    const char *fsrc =
        "uniform mediump vec4 color;\n"
        "void main(void)\n"
        "{\n"
        "   gl_FragColor = color;\n"
        "}";

    if (!fshader->compileSourceCode(fsrc))
    {
      qDebug() << "fshader compile error:\n" << fshader->log();
    }

    QGLShader *vshader = new QGLShader(QGLShader::Vertex, this);
    const char *vsrc =
        "attribute highp vec4 vVertex;\n"
        "void main(void)\n"
        "{\n"
        "    gl_Position = vVertex;\n"
        "}\n";

    vshader->compileSourceCode(vsrc);

    program = new QGLShaderProgram(this);
    program->addShader(vshader);
    program->addShader(fshader);
    program->bindAttributeLocation("vVertex", PROGRAM_VERTEX_ATTRIBUTE);
    program->link();

    program->bind();

    vertexBuffer.create();
    if (!vertexBuffer.isCreated())
    {
      std::cout << "failed creating vertexBuffer" << std::endl;
    }
    vertexBuffer.bind();
    float vs[] = {-1.0f, -1.0f, 0.0f,
                  1.0f, -1.0f, 0.0f,
                  0.0f, 1.0f, 0.0f};
    vertexBuffer.allocate(vs, 9 * sizeof(float));
    glEnable(GL_MULTISAMPLE);
    setAutoFillBackground(false);
    setMinimumSize(200, 200);
    setWindowTitle(tr("triangles and stuff"));
    vertexBuffer.release();
    program->release();

}

void GLWidget::paintGL()
{
  makeCurrent();
  qglClearColor(QColor(0,0,0));
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  program->bind();
  program->setUniformValue("color", 1.0f, 0.0f, 0.0f, 1.0f);
  program->enableAttributeArray(PROGRAM_VERTEX_ATTRIBUTE);
  vertexBuffer.bind();
  program->setAttributeArray(PROGRAM_VERTEX_ATTRIBUTE, (float*)NULL, 3);
  glDrawArrays(GL_TRIANGLES, 0, 3);
  vertexBuffer.release();
  program->disableAttributeArray(PROGRAM_VERTEX_ATTRIBUTE);
  program->release();

  QPainter painter(this);
  //  painter.setRenderHint(QPainter::Antialiasing);
  drawInstructions(&painter);
  painter.end();
}

void GLWidget::resizeGL(int width, int height)
{
  glViewport(0, 0, width, height);
}

void GLWidget::mousePressEvent(QMouseEvent *event)
{
  lastPos = event->pos();
}

void GLWidget::mouseMoveEvent(QMouseEvent *event)
{
  int dx = event->x() - lastPos.x();
  int dy = event->y() - lastPos.y();

  if (event->buttons() & Qt::LeftButton)
  {
  }
  else if (event->buttons() & Qt::RightButton)
  {
  }
  lastPos = event->pos();
}

void GLWidget::mouseReleaseEvent(QMouseEvent * /* event */)
{
  emit clicked();
}

void GLWidget::drawInstructions(QPainter *painter)
{
  QString text = tr("Click and drag with the left mouse button "
                    "to rotate the Qt logo.");
  QFontMetrics metrics = QFontMetrics(font());
  int border = qMax(4, metrics.leading());

  QRect rect = metrics.boundingRect(0, 0, width() - 2*border, int(height()*0.125),
                                    Qt::AlignCenter | Qt::TextWordWrap, text);
  painter->setRenderHint(QPainter::TextAntialiasing);
  painter->fillRect(QRect(0, 0, width(), rect.height() + 2*border),
                    QColor(0, 0, 0, 127));
  painter->setPen(Qt::white);
  painter->fillRect(QRect(0, 0, width(), rect.height() + 2*border),
                    QColor(0, 0, 0, 127));
  painter->drawText((width() - rect.width())/2, border,
                    rect.width(), rect.height(),
                    Qt::AlignCenter | Qt::TextWordWrap, text);
}
