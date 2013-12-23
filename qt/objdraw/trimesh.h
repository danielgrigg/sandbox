#ifndef TRIMESH_H
#define TRIMESH_H

#include <QObject>
#include <QGLBuffer>
#include <QGLShaderProgram>
#include <tr1/memory>
#include <lap/lap.h>
#include <iostream>

#define BUFFER_OFFSET(i) ((char *)NULL + (i))
using namespace std::tr1;
enum
{
  kPosition,
  kNormal,
  kUV
};

namespace lap {
template <> struct VertexPN<int16_t, int16_t>
{
  lap::vec<int16_t, 3> position;
  uint8_t _padding1[2];
  lap::vec<int16_t, 3> normal;
  uint8_t _padding2[2];

  typedef int16_t position_type;
  typedef int16_t normal_type;
  static const bool has_position = true;
  static const bool has_normal = true;

  VertexPN(){}

  template <typename U> VertexPN(const U& rhs):
    position(rhs.position), normal(rhs.normal) { }
};

typedef VertexPN<int16_t,int16_t> VertexP16N16;
}

template <typename T>
GLuint componentType();

template <> inline GLuint componentType<float>() { return GL_FLOAT; }
template <> inline GLuint componentType<int16_t>() { return GL_SHORT; }
template <> inline GLuint componentType<uint8_t>() { return GL_UNSIGNED_BYTE; }

  template<typename V, typename SrcV>
  shared_ptr<lap::Mesh<V> > adapt(shared_ptr<lap::Mesh<SrcV> > src)
  {
    return shared_ptr<lap::Mesh<V> >(new lap::Mesh<V>(*src));
  }

  template <typename V>
  shared_ptr<lap::Mesh<V> > adapt(const shared_ptr<lap::Mesh<V> >& src)
  {
    return src;
  }

template <typename V>
class TriMesh
{
public:


  template <typename SrcV>
  bool init(const shared_ptr<lap::Mesh<SrcV> >& src)
  {
    shared_ptr<lap::Mesh<V> > mesh = adapt<V, SrcV>(src);
    _vertexBuffer.create();
    if (!_vertexBuffer.isCreated()) return false;
    _vertexBuffer.bind();
    _numVertices = mesh->vertices().size();
    _vertexBuffer.allocate(&mesh->vertices()[0], _numVertices * sizeof(V));
    return true;
  }

  void render(QGLShaderProgram *program)
  {
    _vertexBuffer.bind();
    program->enableAttributeArray(kPosition);
    GLuint pt =  componentType<typename V::position_type>();
    glVertexAttribPointer(kPosition, 3,
                          pt,
                          GL_FALSE,
                          sizeof(V),
                          BUFFER_OFFSET(offsetof(V, position)));
    if (V::has_normal)
    {
      GLuint nt = componentType<typename V::normal_type>();
      program->enableAttributeArray(kNormal);
      glVertexAttribPointer(kNormal, 3,
                            nt,
                            GL_FALSE,
                            sizeof(V),
                            BUFFER_OFFSET(offsetof(V, normal)));
    }
    glDrawArrays(GL_TRIANGLES, 0, _numVertices);
    if (V::has_normal)
    {
      program->disableAttributeArray(kNormal);
    }
    program->disableAttributeArray(kPosition);
    _vertexBuffer.release();

  }
private:
  uint32_t _numVertices;
  QGLBuffer _vertexBuffer;
};

#if 0
class TriMesh
{
public:
  TriMesh(){}

  template <typename V>
  bool init(const shared_ptr<lap::Mesh<V> >& mesh)
  {
    _vertexBuffer.create();
    if (!_vertexBuffer.isCreated()) return false;
    _vertexBuffer.bind();
    _numVertices = mesh->vertices().size();
    _vertexSize = sizeof(V);
    _vertexBuffer.allocate(&mesh->vertices()[0], _numVertices * sizeof(V));
    _hasPositions = V::has_position;
    _hasNormals = V::has_normal;
    _positionType = componentType<typename V::position_type>();
    _normalType = componentType<typename V::normal_type>();
    _positionOffset = offsetof(V, position);
    _normalOffset = offsetof(V, normal);
  }

  void render(QGLShaderProgram *program)
  {
    if (_numVertices == 0) return;
    //    GLuint vao;
    //glGenVertexArrays(1, &vao);

    _vertexBuffer.bind();
    if (_hasPositions)
    {
      program->enableAttributeArray(kPosition);
      glVertexAttribPointer(kPosition, 3,
                            _positionType,
                            GL_FALSE,
                            _vertexSize,
                            BUFFER_OFFSET(_positionOffset));
    }

    if (_hasNormals)
    {
      program->enableAttributeArray(kNormal);
      glVertexAttribPointer(kNormal, 3,
                            _normalType,
                            GL_FALSE,
                            _vertexSize,
                            BUFFER_OFFSET(_normalOffset));
    }
    glDrawArrays(GL_TRIANGLES, 0, _numVertices);
    program->disableAttributeArray(kNormal);
    program->disableAttributeArray(kPosition);
  }

private:
  uint32_t _numVertices;
  uint32_t _vertexSize;
  uint32_t _positionOffset;
  uint32_t _normalOffset;
  bool _hasPositions;
  bool _hasNormals;
  GLuint _positionType;
  GLuint _normalType;
  QGLBuffer _vertexBuffer;
};
#endif

#endif // TRIMESH_H
