#ifndef FRAMEBUFFER_H
#define FRAMEBUFFER_H

#include <vector>
#include <string>
#include <boost/function.hpp>
#include <stdint.h>

namespace lgl
{

class ViewFramebuffer
{
  public:
    ViewFramebuffer(uint32_t width, uint32_t height);
    void useWriting();
    uint32_t width()const { return m_width; }
    uint32_t height()const { return m_height; }
  private:
    uint32_t m_width;
    uint32_t m_height;
};

class Renderbuffer
{
  public:
    Renderbuffer();
    ~Renderbuffer();
    void init(uint32_t components, 
        uint32_t width, uint32_t height, uint32_t attachment);
    void destroy();

    uint32_t attachment()const { return m_attachment; }
    uint32_t glname()const { return m_name; }
  private:
    uint32_t m_name;
    uint32_t m_attachment;
};

typedef boost::function<std::vector<Renderbuffer> (uint32_t, uint32_t)> FramebufferBuilder;

std::vector<Renderbuffer> makeRenderbufferD1C3(uint32_t width, uint32_t height);

class Framebuffer
{
  public:
  Framebuffer();
  ~Framebuffer();

  bool init(const std::string& name, 
      uint32_t width, uint32_t height, FramebufferBuilder builder);
   uint32_t width()const { return m_width; }
  uint32_t height()const { return m_height; }

  void destroy();
  void useWriting();
  void useReading(uint32_t renderBufferIndex);

  const std::string& name() const { return m_name; }
  private:

  bool isReady();

  uint32_t m_glname;
  uint32_t m_width;  // Restrict renderbuffer dimensions for simplicity
  uint32_t m_height;
  std::string m_name;
  std::vector<Renderbuffer> m_rbo;
  std::vector<uint32_t> m_colorBuffers;
};
}

#endif
