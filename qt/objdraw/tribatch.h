#ifndef TRIBATCH_H
#define TRIBATCH_H

#include <QObject>
#include <QGLBuffer>
#include <QGLShaderProgram>
#include <tr1/memory>
#include <vector>
#include <lap/lap.h>
#include <iostream>

#if 0
class TriBatch;
typedef shared_ptr<TriBatch> TriBatchPtr;

class TriBatch : public QObject
{
  Q_OBJECT
public:
  explicit TriBatch(QObject *parent = 0);

  bool init(void* vertices, uint32_t numVertices, size_t vertexSize);
  void render(QGLShaderProgram *program);

signals:

public slots:

private:

  uint32_t _numVertices;
  size_t _vertexSize;
  QGLBuffer _vertexBuffer;
  QGLBuffer _elementBuffer;
};
#endif

#if 0
template <typename VertexType, typename OutVertexType>
TriBatchPtr makeTriBatch(shared_ptr<lap::Mesh<VertexType> >& mesh)
{
  lap::Mesh<OutVertexType> outMesh(*mesh);
  std::cout << outMesh << std::endl;
  TriBatchPtr batch(new TriBatch());
  if (!batch->init((void*)&outMesh._vertices[0],
                   outMesh._vertices.size(), sizeof(OutVertexType)))
  {
    return TriBatchPtr();
  }
  return batch;
}
#endif



#endif // TRIBATCH_H
