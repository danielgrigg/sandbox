#include "lineshape.h"

LineShape::LineShape():
_numVertices(),
_vertexSize(),
_vertexBuffer()
{
}

LineShape::~LineShape()
{
}

bool LineShape::init(void* vertices, uint32_t numVertices, size_t vertexSize,
                     const lap::BoundingBox<lap::float3>& bounds)
{
  _bounds = bounds;
  _numVertices = numVertices;
  _vertexSize = vertexSize;
  _vertexBuffer.create();
  if (!_vertexBuffer.isCreated())
  {
    std::cerr << "error creating vertexBuffer" << std::endl;
    return false;
  }
  _vertexBuffer.bind();
  _vertexBuffer.allocate(vertices, numVertices * vertexSize);
  return true;
}

void LineShape::render(QGLShaderProgram *program)
{
  if (_numVertices == 0) return;

  _vertexBuffer.bind();
  program->enableAttributeArray(kPosition);
  program->setAttributeArray(kPosition, GL_FLOAT,
                             BUFFER_OFFSET(offsetof(lap::VertexP, position)),
                             3,
                             _vertexSize);
#if 0
  glDrawArrays(GL_LINE_STRIP, 0, _numVertices);
#else
  glDrawArrays(GL_LINES, 0, _numVertices);
#endif
  program->disableAttributeArray(kPosition);
}
