#include "Framebuffer.h"
#include "../shared/glforward.h"
#include <map>
#include <algorithm>
#include <iostream>
#include <boost/bind.hpp>
#include "GLUtility.h"

namespace lgl
{
  ViewFramebuffer::ViewFramebuffer(uint32_t width, uint32_t height)
  {
    m_width = width;
    m_height = height;
  }

  void ViewFramebuffer::useWriting()
  {
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
    GLenum viewBuffer = GL_BACK_LEFT;
    glDrawBuffers(1, &viewBuffer);
    glViewport(0, 0, m_width, m_height);
  }

  Renderbuffer::Renderbuffer()
  {}

  Renderbuffer::~Renderbuffer()
  {
    destroy();
  }

  void Renderbuffer::init(uint32_t components, 
      uint32_t width, uint32_t height, uint32_t attachment)
  {
    // glGetIntegerv GL_MAX_RENDERBUFFER_SIZE
    // glRenderbufferStorageMultisample
    glGenRenderbuffers(1, &m_name); 
    glBindRenderbuffer(GL_RENDERBUFFER, m_name);
    glRenderbufferStorage(GL_RENDERBUFFER, components, width, height);
    m_attachment = attachment;
  }

  void Renderbuffer::destroy()
  {
    glDeleteRenderbuffers(1, &m_name);
  }


  std::vector<Renderbuffer> makeRenderbufferD1C3(uint32_t width, uint32_t height)
  {
    std::vector<Renderbuffer> buffers(4);
    buffers[0].init(GL_DEPTH_COMPONENT32, width, height, GL_DEPTH_ATTACHMENT);
    buffers[1].init(GL_RGBA8, width, height, GL_COLOR_ATTACHMENT0);
    buffers[2].init(GL_RGBA8, width, height, GL_COLOR_ATTACHMENT1);
    buffers[3].init(GL_RGBA8, width, height, GL_COLOR_ATTACHMENT2);
    return buffers;
  }

  Framebuffer::Framebuffer() 
  { 
    m_glname = m_width = m_height = 0; 
  }

  Framebuffer::~Framebuffer()
  {
    destroy();
  }

  bool Framebuffer::init(const std::string& name, 
      uint32_t width, uint32_t height, FramebufferBuilder builder)
  {
    m_name = name;
    m_width = width;
    m_height = height;
    glGenFramebuffers(1, &m_glname);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, m_glname);
    m_rbo = builder(width, height);
    for (std::vector<Renderbuffer>::const_iterator iter = m_rbo.begin();
        iter != m_rbo.end(); ++iter)
    {
      glFramebufferRenderbuffer(GL_DRAW_FRAMEBUFFER, iter->attachment(),
          GL_RENDERBUFFER, iter->glname());
    }

    checkGLErrors();
    // TODO Awkard, we resolve the colour attachments from the user-given RB layout.
    // The user needs to know how they're mapped though...
    m_colorBuffers.resize(0);
    for (std::vector<Renderbuffer>::const_iterator iter = m_rbo.begin();
        iter != m_rbo.end(); ++iter)
    {
      if (iter->attachment() >= GL_COLOR_ATTACHMENT0 && 
          iter->attachment() <= GL_COLOR_ATTACHMENT15)
      {
        m_colorBuffers.push_back(iter->attachment());
      }
    }

    checkGLErrors();
    return isReady();
  }

  bool Framebuffer::isReady()
  {
    static std::map<GLenum, std::string> codes;
    if (codes.empty())
    {
      codes[GL_FRAMEBUFFER_UNDEFINED] = "undefined";
      codes[GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT] = "incomplete-attachment";
      codes[GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT] = "incomplete-missing-attachment";
      codes[GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER] = "incomplete-draw-buffer";
      codes[GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER] = "incomplete-read-buffer";
      codes[GL_FRAMEBUFFER_UNSUPPORTED] = "unsupported";
      codes[GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE] = "incomplete-multisample";
      codes[GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS] = "incomplete-layer-targets";
    }
    GLenum status = glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
    if (status == GL_FRAMEBUFFER_COMPLETE) 
    {
      return true;
    }
    std::cerr << "Framebuffer " << name() << " error: " << 
      codes[status] << "\n";
    return false;
  }

  void Framebuffer::destroy()
  {
    if (0 == m_glname) return;

    std::for_each(m_rbo.begin(), m_rbo.end(), 
        boost::bind(&Renderbuffer::destroy, _1));
    glDeleteFramebuffers(1, &m_glname);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
  }

  void Framebuffer::useWriting()
  {
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, m_glname);
    glDrawBuffers(m_colorBuffers.size(), &m_colorBuffers[0]);
  }

  //todo - ease association of read-buffer -> render-buffer
  void Framebuffer::useReading(uint32_t renderBufferIndex)
  {
    glBindFramebuffer(GL_READ_FRAMEBUFFER, m_glname);
    glReadBuffer(m_rbo[renderBufferIndex].attachment());
  }
}

