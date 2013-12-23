#include "tribatch.h"

#if 0
TriBatch::TriBatch(QObject *parent) :
    QObject(parent)
{
}

bool TriBatch::init(void* vertices, uint32_t numVertices, size_t vertexSize)
{
  _numVertices = numVertices;
  _vertexSize = vertexSize;
  _vertexBuffer.create();
  if (!_vertexBuffer.isCreated())
  {
    std::cerr << "error creating vertexBuffer" << std::endl;
  }
  _vertexBuffer.bind();
  _vertexBuffer.allocate(vertices, numVertices * vertexSize);
}

void TriBatch::render(QGLShaderProgram* program)
{
  if (_numVertices == 0) return;

  _vertexBuffer.bind();
  program->enableAttributeArray(kPosition);
  program->enableAttributeArray(kNormal);
  GLuint componentType = GL_SHORT;
  using lap::VertexPN;
#if 1
  program->setAttributeArray(kPosition, componentType,
                             BUFFER_OFFSET(offsetof(PositionNormal<float,float>, position)),
                             3,
                             sizeof(Position16Normal16));
  program->setAttributeArray(kNormal, componentType,
                             BUFFER_OFFSET(offsetof(Position16Normal16, normal)),
                             3,
                             sizeof(Position16Normal16));
#else
  glVertexAttribPointer(kPosition, 3, componentType, GL_FALSE,
                        sizeof(Position16Normal16),
                        BUFFER_OFFSET(offsetof(Position16Normal16, position)));
  glVertexAttribPointer(kNormal, 3, componentType, GL_FALSE,
                        sizeof(Position16Normal16),
                        BUFFER_OFFSET(offsetof(Position16Normal16, normal)));
#endif

  glDrawArrays(GL_TRIANGLES, 0, _numVertices);

  program->disableAttributeArray(kNormal);
  program->disableAttributeArray(kPosition);
}
#endif
