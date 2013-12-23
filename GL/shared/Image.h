//
//  Image.h
//  dump_glyphmap
//
//  Created by Daniel Grigg on 2/04/11.
//  Copyright 2011 Daniel Grigg. All rights reserved.
//

#ifndef IMAGE_H
#define IMAGE_H

#include <stdint.h>
#include <vector>
#include <string>
#include "../shared/Tga.h"

struct RGB 
{ 
  RGB():r(0),g(0),b(0){} 
  RGB(uint8_t vr, uint8_t vg, uint8_t vb):
  r(vr),g(vg),b(vb){} 
  uint8_t r, g, b; 

  void op_or(const RGB& other)
  {
    r |= other.r;
    g |= other.g;
    b |= other.b;
  }
};

template<typename T>
class Image
{
public:
  Image()
  {
    m_width = 0;
    m_height = 0;
  }
  
  Image(int vWidth, int vHeight, const T background=T()):
  m_width(vWidth),
  m_height(vHeight)
  {
    m_pixels.assign(width() * height(), background);
  }
  void setPixel(int x, int y, const T& color)
  {
    if (x < 0 || x >= width() || y < 0 || y >= height()) return;
    //m_pixels[y * width() + x].op_or(color);
    m_pixels[y * width() + x] = (color);
  }
  
  T& pixel(int x, int y) { return m_pixels[y * width() + x]; }
  T pixel(int x, int y)const { return m_pixels[y * width() + x]; }
  const int width()const { return m_width; }
  const int height()const { return m_height; }
  const std::vector<T> & pixels()const { return m_pixels; }
  

  void drawImage(const Image<T>& image, int xOrigin, int yOrigin)
  {
    for (int y = 0; y < image.height(); ++y)
    {
      for (int x = 0; x < image.width(); ++x)
      {
        setPixel(xOrigin + x, yOrigin + y, image.pixel(x,y));
      }
    }
  }
  
  
  bool save(const std::string& fileName)
  {
    return gltWriteTGA(fileName.c_str(), sizeof(T)*8, width(), height(),
                (const uint8_t*)&m_pixels[0]);
  }
  
private:
  
  int m_width;
  int m_height;
  std::vector<T> m_pixels;
};

#endif
