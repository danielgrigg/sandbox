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

using namespace lap;
using namespace std;
using namespace std::tr1;

int gMVP = 0;

static const char* kAttributes[] = {"vPosition", "vNormal"};

Scene::Scene(QObject *parent):
    QGraphicsScene(parent)
{
    QWidget *instructions = createDialog(tr("Instructions"));
    _label = new QLabel(tr("This is a dialog box!"));
    instructions->layout()->addWidget(_label);
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
        "varying vec3 normal;\n"
        "void main(void)\n"
        "{\n"
        "		vec3 C = vec3(.5) + .5 * normalize(normal);\n"
        "   gl_FragColor = vec4(C, 1.0);\n"
        "}";

    if (!fshader->compileSourceCode(fsrc))
    {
      qDebug() << "fshader log:\n" << fshader->log();
    }

    QGLShader *vshader = new QGLShader(QGLShader::Vertex, this);
    const char *vsrc =
        "uniform mat4 mvpMatrix;"
        "attribute highp vec4 vPosition;\n"
        "attribute highp vec3 vNormal;\n"
        "varying vec3 normal;\n"
        "void main(void)\n"
        "{\n"
        "			normal = vNormal;\n"
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
    program->bindAttributeLocation(kAttributes[kNormal], kNormal);
    if (!program->link())
    {
      qDebug() << program->log();
    }
    gMVP =	program->uniformLocation("mvpMatrix");

    createMesh();
    _appTime.start();
    m_timer = new QTimer(this);
    m_timer->setInterval(17);
    connect(m_timer, SIGNAL(timeout()), this, SLOT(update()));
    m_timer->start();
}

template <typename Vout, typename Vin>
shared_ptr<TriMesh<Vout> > makeTriMesh(const shared_ptr<Mesh<Vin> >& src)
{
  shared_ptr<TriMesh<Vout> > mesh(new TriMesh<Vout>());
  if (!mesh->init(src))
  {
    return shared_ptr<TriMesh<Vout> >();
  }
  return mesh;
}

template<typename MeshType>
void Scene::buildMesh(shared_ptr<MeshType> src)
{
  //shared_ptr<MeshType> mesh = indexedMeshFromMesh( src->flatten());
  shared_ptr<MeshType> mesh = lap::meshFromIndexedMesh(lap::indexedMeshFromMesh(src->flatten()));
//  shared_ptr<MeshType> mesh = src;
  cout << "Mesh: " << mesh->triangles() << " triangles\n";
  //_batch = makeTriMesh<VertexP16N16>(src);
  _batch = makeTriMesh<VertexPNf>(src);
}

bool Scene::createMesh()
{
  std::string modelFile = "/Users/daniel/content/models/artstudio_pn.obj";
//  std::string modelFile = "/Users/daniel/content/models/test/sphere.obj";

//  std::string modelFile = "/Users/daniel/content/scenes/Bedroom/Bedroom.obj";
  obj::ModelPtr model = obj::ObjTranslator().importFile(modelFile);
  if (!model)
  {
    cerr << "Error importing " << modelFile << endl;
    return false;
  }
  std::cout << "Imported " << modelFile << endl;
  //cout << *model << endl;

  cout << "vformat " << model->vertexFormat() << endl;
  switch (model->vertexFormat())
  {
  //  case obj::kPosition: buildMesh(meshFromObj<VertexPf>(model)); break;
  //  case obj::kPositionUV: buildMesh(meshFromObj<VertexPTf>(model)); break;
  case obj::kPositionNormal: buildMesh(meshFromObj<VertexPNf>(model)); break;
  case obj::kPositionUVNormal: buildMesh(meshFromObj<VertexPTNf>(model)); break;
  default: cerr << "Invalid vertex format" << endl; break;
  }
  return true;
}
QDialog *Scene::createDialog(const QString &windowTitle) const
{
  QDialog *dialog = new QDialog(0, Qt::CustomizeWindowHint | Qt::WindowTitleHint);

  dialog->setWindowOpacity(0.8);
  dialog->setWindowTitle(windowTitle);
  dialog->setLayout(new QVBoxLayout);

  return dialog;
}

void Scene::render()
{
  program->bind();
  QMatrix4x4 m;
  //  m.translate(0, 0, -0);
  float r = _appTime.elapsed() / 1000.0f;
  std::string s = boost::lexical_cast<string>(_appTime.elapsed() / 1000);
  _label->setText(QString::fromStdString(s));
 m.rotate(0.1f * r, 0, 1, 0);
  QMatrix4x4 mvp;
  mvp.perspective(38.0f, 1.0f, 10.0f, 10000.0f);
  mvp.lookAt(QVector3D(-684,283,300), QVector3D(-1274,190,235), QVector3D(0, 1, 0));
 // mvp.lookAt(QVector3D(7,7,7), QVector3D(0,0,0), QVector3D(0, 1, 0));
  mvp *= m;
  program->setUniformValue("mvpMatrix", mvp);

  _batch->render(program);
  program->release();
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
  glClearColor(0.2, 0.2, 0.2, 1.0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glEnable(GL_DEPTH_TEST);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();

  glEnable(GL_MULTISAMPLE);

  render();

  glDisable(GL_MULTISAMPLE);

  glPopMatrix();
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();

  painter->endNativePainting();
}
