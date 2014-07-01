#include <algorithm>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <map>
#include <numeric>
#include <sstream>
#include <stdint.h>
#include <string>
#include <vector>
#include <tr1/memory>
#include <cassert>
#include <utility>
#include <queue>
#include "Tga.h"

using namespace std;
using namespace std::tr1;

struct Rect
{
  Rect()
  {
    m_x0 = m_y0 = m_width = m_height = 0;
  }
  Rect(uint16_t x, uint16_t y, uint16_t w, uint16_t h)
  {
    m_x0 = x; m_y0 = y; m_width = w; m_height = h;
  }
  
  uint16_t x0()const { return m_x0; }
  uint16_t y0()const { return m_y0; }
  uint16_t x1()const { return m_x0 + m_width; }
  uint16_t y1()const { return m_y0 + m_height; }
  uint16_t width()const { return m_width; }
  uint16_t height()const { return m_height; }
  
  bool fits(const Rect other)
  {
    return (other.width() <= width() && other.height() <= height());
  }
  
  uint32_t area()const { return width() * height(); }
  
  bool operator==(const Rect rhs)const 
  { 
    return (x0() == rhs.x0() && y0() == rhs.y0() && 
            x1() == rhs.x1() && y1() == rhs.y1());
  }
  bool intersects(const Rect other)const
  {
    return !(x0() >= other.x1() || other.x0() >= x0() ||
             y0() >= other.y1() || other.y0() >= y1());
  }
  
private:
  uint16_t m_x0, m_y0;
  uint16_t m_width, m_height;
  
};



std::ostream& operator<<(std::ostream& os, const Rect&rhs)
{
  os << "[(" << rhs.x0() << ", " << rhs.y0() << "),(" 
  << rhs.x1() << ", " << rhs.y1() << ")]";
  return os;
}

uint32_t random(int minValue, int maxValue)
{
  //assert(maxValue > minValue);
  float t = (float)(rand() ^ (rand() + 271)) / (float)RAND_MAX;
  return (1.0f - t) * minValue + t * maxValue;
//  return minValue + rand() % (maxValue-minValue);
}

Rect makeRandomRect()
{
  Rect R(0, 0, random(4, 80), random(4, 80));
  assert(R.area() > 0);
  return R;
}
struct Node;

typedef shared_ptr<Node> NodePtr;

struct Node 
{  
  Node(const Rect bounds):
  m_bounds(bounds)
  {
    m_rectId = -1;
  }
  
  bool isLeaf()const { return !m_children[0] && !m_children[1]; }
  
  bool insert(uint32_t rectId, const Rect rect)
  { 
    assert(rect.area() > 0);
    if (m_rectId != -1) return false;
    
    // Perfect containment, we're done inserting.
    if (rect.width() == m_bounds.width() && rect.height() == m_bounds.height())
    {
      m_rectId = rectId;
      return true;
    }
    // Don't fit the current node, backtrack.
    if (!m_bounds.fits(rect)) return false;
    
    // Leaf case, split the space. Doing this scenario first means we can 
    // generalise the 'recurse' scenarios.
    if (isLeaf())
    {
      // Prefer square(ish) nodes by splitting along longest axis.
      
      if ((m_bounds.width() > m_bounds.height() && 
           m_bounds.width() > rect.width()) ||
          m_bounds.height() == rect.height())
      {
        // Vertical split
        m_children[0] = NodePtr(new Node(Rect(m_bounds.x0(),
                                              m_bounds.y0(),
                                              rect.width(),
                                              m_bounds.height())));
        m_children[1] = NodePtr(new Node(Rect(m_bounds.x0()+rect.width(),
                                              m_bounds.y0(),
                                              m_bounds.width()-rect.width(), 
                                              m_bounds.height())));
      }
      else
      {
        m_children[0] = NodePtr(new Node(Rect(m_bounds.x0(),
                                              m_bounds.y0(),
                                              m_bounds.width(),
                                              rect.height())));
        m_children[1] = NodePtr(new Node(Rect(m_bounds.x0(),
                                              m_bounds.y0()+rect.height(),
                                              m_bounds.width(), 
                                              m_bounds.height()-rect.height())));
      }
    }
    if (m_children[0] && m_children[0]->insert(rectId, rect)) return true;    
    if (m_children[1] && m_children[1]->insert(rectId, rect)) return true;
    return false;
  }
  
  string toString(int depth)const;
  Rect m_bounds;
  uint32_t m_rectId;
  shared_ptr<Node> m_children[2];
  
};

ostream &operator<<(ostream& os, const Node& rhs)
{
  os << rhs.toString(0);
  return os;
}

string Node::toString(int depth)const
{
  ostringstream oss;
  oss << "\n";
  for (int i =0; i < 2*depth; ++i) {
    oss << ' ';
  }
  oss << m_bounds;
  if (m_rectId != -1)
  {
    oss << " (" << m_rectId << ")";
  }
  if (m_children[0]) oss << m_children[0]->toString(depth+1);
  if (m_children[1]) oss << m_children[1]->toString(depth+1);
  
  return oss.str();
}

struct RGB 
{ 
  RGB():r(0),g(0),b(0){} 
  RGB(uint8_t vr, uint8_t vg, uint8_t vb):
  r(vr),g(vg),b(vb){} 
  uint8_t r, g, b; 
};

struct ImageRGB
{
public:
  ImageRGB()
  {
    m_width = 0;
    m_height = 0;
  }
  
  ImageRGB(int vWidth, int vHeight, const RGB background = RGB()):
  m_width(vWidth),
  m_height(vHeight)
  {
    m_pixels.assign(width() * height(), background);
  }
  
  void setPixel(int x, int y, const RGB& color)
  {
    if (x < 0 || x >= width() || y < 0 || y >= height()) return;
    m_pixels[y * width() + x].r =  color.r;
    m_pixels[y * width() + x].g = color.g;
    m_pixels[y * width() + x].b = color.b;
  }
  
  RGB& pixel(int x, int y) { return m_pixels[y * width() + x]; }
  RGB pixel(int x, int y)const { return m_pixels[y * width() + x]; }
  
  void drawImage(const ImageRGB& image, int xOrigin, int yOrigin)
  {
    for (int y = 0; y < image.height(); ++y)
    {
      for (int x = 0; x < image.width(); ++x)
      {
        setPixel(xOrigin + x, yOrigin + y, image.pixel(x,y));
      }
    }
  }
  
  const int width()const { return m_width; }
  const int height()const { return m_height; }
  const std::vector<RGB> & pixels()const { return m_pixels; }
  
  void save(const std::string& fileName)
  {
    std::vector<uint8_t> outPixels(m_pixels.size() * sizeof(RGB));
    memcpy(&outPixels[0], &m_pixels[0], outPixels.size());
    if (width() > 0 && height() > 0)
    {
      gltWriteTGA(fileName.c_str(), width(), height(), outPixels);
    }
  }
  
private:
  
  int m_width;
  int m_height;
  std::vector<RGB> m_pixels;
};



void visitNode(const NodePtr node, ImageRGB& canvas, int numRects, 
               vector<Rect>& allBounds)
{
//  cout << node->m_bounds << endl;
  
  if (node->m_rectId != -1)
  {
  //  cout << "Drawing " << node->m_bounds << endl;
//    float t = (float)node->m_rectId / (float)numRects;
//    RGB C(t * 255.5f, 0 * 255.5f, t * 255.0f);
    RGB C(random(0,256), random(0,256), random(0,256));
    ImageRGB nodeImage(node->m_bounds.width(), node->m_bounds.height(), C);
    canvas.drawImage(nodeImage, node->m_bounds.x0(), node->m_bounds.y0());
    
    allBounds.push_back(node->m_bounds);
  }
}

bool areaLess(const Rect lhs, const Rect rhs) { return lhs.area() < rhs.area(); }  
bool areaGE(const Rect lhs, const Rect rhs) { return lhs.area() >= rhs.area(); }
int areaGE_q(const void* lhs, const void* rhs) 
{ 
  const Rect* a = (Rect*)lhs; const Rect* b = (Rect*)rhs;
  return b->area() - a->area();
}

#include <stdlib.h>

void breadthTraversal(NodePtr root, ImageRGB& canvas, vector<Rect>& allBounds,
                      int numRects)
{
  queue<NodePtr> nodes;
  nodes.push(root);
  do {
    NodePtr current = nodes.front();
    visitNode(current, canvas, numRects, allBounds);
    nodes.pop();
    if (current->m_children[0]) nodes.push(current->m_children[0]);
    if (current->m_children[1]) nodes.push(current->m_children[1]);
  } while (!nodes.empty());
}

int main(int argc, char **argv)
{  
  srand(time(NULL));
  const int numRects = argc > 1 ? strtol(argv[1], NULL, 10) : 1000000;
  std::vector<Rect> rects(numRects);
  std::generate(rects.begin(), rects.end(), makeRandomRect);
   
//  std::sort(rects.begin(), rects.end(), areaGE);
  qsort(&rects[0], rects.size(), sizeof(Rect), areaGE_q);
  
//  ostream_iterator<Rect> osi(cout, "\n");
//  copy(rects.begin(), rects.end(), osi);
//  cout << endl;

  const int w = 1000;
  const int h = 1000;
  Rect worldBounds(0,0,1000,1000);
  NodePtr world = NodePtr(new Node(worldBounds));
  
  for (uint32_t i = 0; i < rects.size(); ++i) 
  {
    world->insert(i, rects[i]);
//    if (!world->insert(i, rects[i])) cout << "Rejected " << i << endl;
  }
//  cout << *world << endl;
  
  ImageRGB image(w, h, RGB(255,255,255));
  
  vector<Rect> allBounds;
  allBounds.reserve(1000);
  breadthTraversal(world, image, allBounds, numRects);
  
  for (uint32_t i = 0; i < allBounds.size(); ++i) {
    Rect& current = allBounds[i];
    for (uint32_t j = 0; j < i; ++j) {
      Rect& other = allBounds[j];
      if (current.intersects(other))
      {
        cout << current << " -> " << other << endl;
      }
      
    }
  }
  
  image.save("packed.tga");
  
  return 0;
}




