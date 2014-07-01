#ifndef LINESHAPE_H
#define LINESHAPE_H

#include <stdint.h>
#include <tr1/memory>
#include <vector>
#include <lap/lap.h>
#include <QGLBuffer>
#include <QGLShaderProgram>
#include <algorithm>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/lambda.hpp>

#define BUFFER_OFFSET(i) ((char *)NULL + (i))
using namespace std::tr1;
using namespace boost::lambda;

class LineShape;
typedef shared_ptr<LineShape> LineShapePtr;

enum
{
  kPosition,
  kNormal
};

static const char* kAttributes[] = {"vPosition", "vNormal"};

class LineShape
{
public:
    LineShape();
    ~LineShape();
    bool init(void* vertices, uint32_t numVertices, size_t vertexSize,
      const lap::BoundingBox<lap::float3>& bounds);
    void render(QGLShaderProgram *program);
    const lap::BoundingBox<lap::float3>& bounds()const { return _bounds; }
private:

  uint32_t _numVertices;
  size_t _vertexSize;
  QGLBuffer _vertexBuffer;
  QGLBuffer _elementBuffer;
  lap::BoundingBox<lap::float3> _bounds;
};

template <typename V>
void unionPoint(lap::BoundingBox<lap::float3>& bounds, const V& v)
{
  bounds.unionPoint(v.position);
}

template <typename VertexType>
LineShapePtr makeLineShape(shared_ptr<lap::Mesh<VertexType> > mesh)
{
  lap::BoundingBox<lap::float3> bounds;
  std::for_each(mesh->_vertices.begin(), mesh->_vertices.end(),
                bind(unionPoint<VertexType>, ref(bounds), boost::cref(_1)));
  LineShapePtr batch(new LineShape());
  if (!batch->init((void*)&mesh->_vertices[0],
                   mesh->_vertices.size(), sizeof(VertexType),
                   bounds))
  {
    return LineShapePtr();
  }

  std::cout << "vertices " << mesh->_vertices.size() << std::endl;
  return batch;
}


#endif // LINESHAPE_H
