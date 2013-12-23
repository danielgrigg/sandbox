//
//  Packer.cpp
//  dump_glyphmap
//
//  Created by Daniel Grigg on 2/04/11.
//  Copyright 2011 Daniel Grigg. All rights reserved.
//

#include "FontCache.h"
#include <tr1/memory>
#include <stdint.h>
#include <string>
#include <sstream>
#include <queue>
#include <cassert>
#include <vector>
#include <iostream>
#include <boost/bind.hpp>
#include <numeric>
#include <math.h>

class Node;
typedef std::tr1::shared_ptr<Node> NodePtr;

class Node : public std::tr1::enable_shared_from_this<Node>
{
public:
  class Visitor
  {
  public:
    virtual ~Visitor(){}
    
    virtual void visit(const NodePtr node) = 0;
  };
  
  class Rect
  {
  public:
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
  
  Node(const Rect bounds):
  m_bounds(bounds)
  {
    m_itemId = -1;
  }
  
  bool hasItem()const { return m_itemId != ~0U; }
  bool isLeaf()const { return !m_children[0] && !m_children[1]; }
  
  bool insert(uint32_t itemId, const Rect rect)
  { 
    assert(rect.area() > 0);
    if (hasItem()) return false;
    
    // Perfect containment, we're done inserting.
    if (rect.width() == m_bounds.width() && rect.height() == m_bounds.height())
    {
      m_itemId = itemId;
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
    if (m_children[0] && m_children[0]->insert(itemId, rect)) return true;    
    if (m_children[1] && m_children[1]->insert(itemId, rect)) return true;
    return false;
  }
  
  void breadthTraverse(Node::Visitor& visitor)
  {
    std::queue<NodePtr> nodes;
    nodes.push(shared_from_this());
    do 
    {
      NodePtr current = nodes.front();
      visitor.visit(current);
      nodes.pop();
      if (current->m_children[0]) nodes.push(current->m_children[0]);
      if (current->m_children[1]) nodes.push(current->m_children[1]);
    } while (!nodes.empty());
  }
  
  
  std::string toString(int depth)const;
  Rect m_bounds;
  uint32_t m_itemId;
  NodePtr m_children[2];
  
};

int areaGE_q(const void* lhs, const void* rhs) 
{ 
  const Node::Rect* a = (Node::Rect*)lhs; 
  const Node::Rect* b = (Node::Rect*)rhs;
  return b->area() - a->area();
}


std::ostream& operator<<(std::ostream& os, const Node::Rect& rhs)
{
  os << "[(" << rhs.x0() << ", " << rhs.y0() << "),(" 
  << rhs.x1() << ", " << rhs.y1() << ")]";
  return os;
}

std::ostream &operator<<(std::ostream& os, const Node& rhs)
{
  os << rhs.toString(0);
  return os;
}

std::string Node::toString(int depth)const
{
  std::ostringstream oss;
  oss << "\n";
  for (int i =0; i < 2*depth; ++i) {
    oss << ' ';
  }
  oss << m_bounds;
  if (hasItem())
  {
    oss << " (" << m_itemId << ")";
  }
  if (m_children[0]) oss << m_children[0]->toString(depth+1);
  if (m_children[1]) oss << m_children[1]->toString(depth+1);
  
  return oss.str();
}

int glyphAreaCompare(const void* a, const void* b)
{
  const Glyph* lhs = (const Glyph*)a;
  const Glyph* rhs = (const Glyph*)b;
  return rhs->area() - lhs->area();
}

std::ostream& operator<<(std::ostream& os, const IndexedGlyph& rhs)
{
  os << "([" << rhs.u0() << ", " << rhs.v0() << "], [" << rhs.u1() << ", " << 
  rhs.v1() << "], " << rhs.layout() << ")";
  return os;
}


class DrawVisitor : public Node::Visitor
{
public:
  DrawVisitor(Image<uint8_t>* canvas, std::vector<Glyph>* alphabet):
  m_canvas(canvas),
  m_alphabet(alphabet)
  {}
  virtual void visit(const NodePtr node)
  {
    if (node->hasItem())
    {
      const Glyph& g = (*m_alphabet)[node->m_itemId];
      Image<uint8_t> nodeImage(node->m_bounds.width(), node->m_bounds.height());
      m_canvas->drawImage(nodeImage, node->m_bounds.x0(), node->m_bounds.y0());
      m_canvas->drawImage(g.bitmap(), node->m_bounds.x0(), node->m_bounds.y0());
    }
  }
private:
  Image<uint8_t>* m_canvas;
  std::vector<Glyph>* m_alphabet;
};

class IndexerVisitor : public Node::Visitor
{
public:
  IndexerVisitor(std::vector<Glyph>* alphabet , 
                 std::vector<IndexedGlyph>* indexedGlyphs):
  m_alphabet(alphabet),
  m_indexedGlyphs(indexedGlyphs)
  {
  }

  virtual void visit(const NodePtr node)
  {
    if (node->hasItem())
    {
      const Glyph& g = (*m_alphabet)[node->m_itemId];
      (*m_indexedGlyphs)[g.charCode()] = 
        IndexedGlyph(node->m_bounds.x0(), node->m_bounds.y0(), 
                     node->m_bounds.x1(), node->m_bounds.y1(),
                     g.layout());
    }
  }
  std::vector<Glyph>* m_alphabet;
  std::vector<IndexedGlyph>* m_indexedGlyphs;
};


// alphabet is sorted by area in descending order.
NodePtr makeGlyphTree(const std::vector<Glyph>& alphabet,
                      uint16_t xMax, uint16_t yMax)
{
  NodePtr tree(new Node(Node::Rect(0,0,xMax,yMax)));
  for (uint32_t i = 0; i < alphabet.size(); ++i) 
  {
    const Glyph& g = alphabet[i];
    // Ignore glyphs without bitmaps.
    if (g.area() == 0) continue;

    if (!tree->insert(i, 
          Node::Rect(0, 0, 
            g.bitmap().width(), g.bitmap().height())))
    {
      return NodePtr();
    }
  }
  return tree;
}

uint32_t addGlyphArea(uint32_t sum, const Glyph&rhs)
{
  return sum + rhs.area();
}

uint32_t addGlyphWidth(uint32_t sum, const Glyph&rhs)
{
  return sum + rhs.bitmap().width();
}
uint32_t addGlyphHeight(uint32_t sum, const Glyph&rhs)
{
  return sum + rhs.bitmap().height();
}

IndexedGlyph makeDefaultIndexedGlyph(Glyph& prototype)
{
  return IndexedGlyph(prototype.layout());
}

bool makeFontCache(const FontFace& descriptor,
                   Image<uint8_t>& canvas, 
                   std::vector<IndexedGlyph>& index)
{
  std::vector<Glyph> alphabet(descriptor.alphabet());
  index.resize(alphabet.size());
  std::transform(alphabet.begin(), alphabet.end(), index.begin(), 
      makeDefaultIndexedGlyph);

  // Our tree requires sorted glyphs.
  qsort(&alphabet[0], alphabet.size(), sizeof(Glyph), glyphAreaCompare);
  
  uint32_t sumWidth = std::accumulate(alphabet.begin(), alphabet.end(), 
      0, addGlyphWidth);
  uint32_t sumHeight = std::accumulate(alphabet.begin(), alphabet.end(), 
      0, addGlyphHeight);
  float aspect = (float)sumWidth / (float)sumHeight;
  std::cout << "Aspect: " << aspect << std::endl;
  uint32_t alphabetArea = std::accumulate(alphabet.begin(), alphabet.end(), 0, 
                                          addGlyphArea);                      
  std::cout << "Min area: " << alphabetArea << std::endl;
  uint16_t l = sqrtf(alphabetArea);
//  uint16_t x = (float)l / aspect;  uint16_t y = (float)l*aspect;
  float s = 1.05f;
  int xMax = 0; int yMax = 0;
  NodePtr glyphTree;
  do
  {
    xMax = s * l;
    yMax = s * l;
    std::cout << "xMax: " << xMax << ", yMax: " << yMax << std::endl;
    std::cout << "Max area: " << xMax*yMax << std::endl;
    std::cout << "Efficiency: " << (float)alphabetArea / (float)(xMax*yMax) << std::endl; 
    glyphTree = makeGlyphTree(alphabet, xMax, yMax);
    if (!glyphTree) 
    {
      std::cerr << "Font doesn't fit! Increasing cache size" << std::endl;
      s += 0.01f;
    }
  }
  while (!glyphTree);
  canvas = Image<uint8_t>(xMax, yMax);
  DrawVisitor visitor(&canvas, &alphabet);
  glyphTree->breadthTraverse(visitor);
  IndexerVisitor indexer(&alphabet, &index);
  glyphTree->breadthTraverse(indexer);
  return true;
}
