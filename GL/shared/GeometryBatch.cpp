#include "GeometryBatch.h"

#undef glGenVertexArrays
#undef glDeleteVertexArrays
#undef glBindVertexArray
#define glGenVertexArrays glGenVertexArraysAPPLE
#define glDeleteVertexArrays  glDeleteVertexArraysAPPLE
#define glBindVertexArray	glBindVertexArrayAPPLE

#define BUFFER_OFFSET(i) ((char *)NULL + (i))

void GeometryBatch::init(GLuint prim, const string &name, Usage usage)
{
  bound = false;
  primitiveType = prim;
  this->name = name;
  glGenVertexArrays(1, &arrayObject);
  glBindVertexArray(arrayObject);
  glGenBuffers(1, &vertexObject);
  glGenBuffers(1, &colorObject);
  glGenBuffers(1, &normalObject);
  glGenBuffers(kUVMax, uvObjects);
  glGenBuffers(1, &elementObject);

  uint32_t usageMap[kUsageMax] = {GL_STATIC_DRAW, m_usage};
  m_usage = usageMap[usage];
}

void GeometryBatch::draw() 
{ 
  if (!elements.empty())
  {
    GLuint elementType = sizeof(ElementType) == 2 ? 
      GL_UNSIGNED_SHORT : GL_UNSIGNED_INT;
    // Assume the batch is a single primitive
    glDrawRangeElements(primitiveType, 0, ps.size(), elements.size(), 
        elementType, 0);
  }
  else
  {
    glDrawArrays(primitiveType, 0, ps.size()); 
  }
}

void GeometryBatch::drawRange(uint32_t offset, uint32_t count)
{
  drawRange(offset, count, 0, ps.size());
}

void GeometryBatch::drawRange(uint32_t offset, uint32_t count, 
    uint32_t minElement, uint32_t maxElement)
{
  assert(!elements.empty());
  GLuint elementType = sizeof(ElementType) == 2 ? 
    GL_UNSIGNED_SHORT : GL_UNSIGNED_INT;
  glDrawRangeElements(primitiveType, minElement, maxElement, count, elementType, 
      BUFFER_OFFSET(offset * sizeof(ElementType)));
}

void GeometryBatch::bind()
{
  glBindVertexArray(arrayObject);
  if (ps.size() > 0)
  {
    glEnableVertexAttribArray(kPosOffset);
    glBindBuffer(GL_ARRAY_BUFFER, vertexObject);
    if (!bound)
    {
      glBufferData(GL_ARRAY_BUFFER, sizeof(LPoint) * ps.size(), &ps[0], m_usage);
      glVertexAttribPointer(kPosOffset, sizeof(LPoint)/sizeof(float), GL_FLOAT, GL_FALSE, 0, 0);
    }
  }
  else
  {
    glDisableVertexAttribArray(kPosOffset);
  }

  if (ns.size() > 0)
  {
    glEnableVertexAttribArray(kNormalOffset);
    glBindBuffer(GL_ARRAY_BUFFER, normalObject);
    if (!bound)
    {
      glBufferData(GL_ARRAY_BUFFER, sizeof(LNormal) * ns.size(), &ns[0], m_usage);
      glVertexAttribPointer(kNormalOffset, sizeof(LNormal)/sizeof(float), GL_FLOAT, GL_FALSE, 0, 0);
    }
  }
  else
  {
    glDisableVertexAttribArray(kNormalOffset);
  }


  if (cs.size() > 0)
  {
    glEnableVertexAttribArray(kColorOffset);
    glBindBuffer(GL_ARRAY_BUFFER, colorObject);
    if (!bound)
    {
      glBufferData(GL_ARRAY_BUFFER, sizeof(LColor) * cs.size(), &cs[0], m_usage);
      glVertexAttribPointer(kColorOffset, sizeof(LColor)/sizeof(uint8_t), GL_UNSIGNED_BYTE, GL_TRUE, 0, 0);
    }
  }
  else
  {
    glDisableVertexAttribArray(kColorOffset);
  }

  for (int i = 0; i < kUVMax; ++i)
  {
    if (uvs[i].size() > 0)
    {
      glEnableVertexAttribArray(kUV0Offset + i);
      glBindBuffer(GL_ARRAY_BUFFER, uvObjects[i]);
      if (!bound)
      {
        glBufferData(GL_ARRAY_BUFFER, sizeof(UV) * uvs[i].size(), &uvs[i][0], m_usage);
        glVertexAttribPointer(kUV0Offset + i, sizeof(UV)/sizeof(float), GL_FLOAT, GL_FALSE, 0, 0);
      }
    }
    else
    {
      glDisableVertexAttribArray(kUV0Offset + i);
    }
  }
  if (!elements.empty())
  {
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementObject);
    if (!bound)
    {
      glBufferData(GL_ELEMENT_ARRAY_BUFFER, 
          sizeof(ElementType) * elements.size(), &elements[0], m_usage);
    }
  }
  bound = true;
}

