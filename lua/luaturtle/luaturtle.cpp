/*  \author Daniel Grigg                                                                       
 *  \date 22/05/11.
 *  luaturtle
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 *  Implements a simple Turtle-like DSL in LUA against a C++ backend.  The Lua
 *  files specify turtle commands such as forward, left, etc while the backend
 *  maintains the turtle's state, interprets commands and renders the results
 *  to a TGA image.
 */
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <math.h>
#include <stdlib.h>
#include "Tga.h"
#include <tr1/memory>
#include <lua.hpp>

using namespace std;

float toRadians(float degrees)
{
  return 3.141592653589793 * degrees / 180.0f;
}

bool fileExists(const std::string &path)                                                   
{
  std::ifstream ifs(path.c_str());
  return ifs.is_open();
}

struct RGB 
{ 
  RGB():r(0),g(0),b(0){} 
  RGB(uint8_t vr, uint8_t vg, uint8_t vb):
    r(vr),g(vg),b(vb){} 
  uint8_t r, g, b; 
};

// Cute little canvas for line-drawing.
template<typename T>
class Canvas
{
  public:
    Canvas()
    {
      m_width = 0;
      m_height = 0;
    }

    Canvas(int vWidth, int vHeight, const T background=T()):
      m_width(vWidth),
      m_height(vHeight)
  {
    m_pixels.assign(width() * height(), background);
  }
    void setPixel(int x, int y, const T& color)
    {
      if (x < 0 || x >= width() || y < 0 || y >= height()) return;
      m_pixels[y * width() + x] = (color);
    }

    T& pixel(int x, int y) { return m_pixels[y * width() + x]; }
    T pixel(int x, int y)const { return m_pixels[y * width() + x]; }
    const int width()const { return m_width; }
    const int height()const { return m_height; }

    void drawLine(int x0, int y0, int x1, int y1)
    {
      int dx = abs(x1-x0), dy = abs(y1-y0);
      int sx = x0 < x1 ? 1 : -1;
      int sy = y0 < y1 ? 1 : -1;
      int err = dx-dy;
      while (true)
      {
        setPixel(x0, y0, RGB(255,255,255));
        if (x0 == x1 && y0 == y1) break;
        int e2 = 2*err;
        if (e2 > -dy)
        {
          err = err - dy;
          x0 = x0 + sx;
        }
        if (e2 < dx)
        {
          err = err + dx;
          y0 = y0 + sy;
        }
      } 
    }

    bool save(const std::string& fileName)
    {
      return TgaWrite(fileName.c_str(), sizeof(T)*8, width(), height(),
          (const uint8_t*)&m_pixels[0]);
    }
  private:
    int m_width;
    int m_height;
    std::vector<T> m_pixels;
};

typedef std::tr1::shared_ptr<Canvas<RGB> > CanvasPtr;
CanvasPtr gCanvas;

// The 'model'
struct Turtle
{
  float x;
  float y;
  int heading;
};

static Turtle gTurtle = {0.0f,0.0f,0.0f};

// Move the Turtle forward without drawing
int Forward(int distance)
{
  float heading_r = toRadians(gTurtle.heading);
  gTurtle.x = (int)(gTurtle.x + (float)distance * cos(heading_r) + 0.5f);
  gTurtle.y = (int)(gTurtle.y + (float)distance * sin(heading_r) + 0.5f);
  return distance;
}

// Move the Turtle forward whilst drawing
int Draw(int distance)
{
  float heading_r = toRadians(gTurtle.heading);
  int x1 = (int)(gTurtle.x + (float)distance * cos(heading_r) + 0.5f);
  int y1 = (int)(gTurtle.y + (float)distance * sin(heading_r) + 0.5f);
  gCanvas->drawLine(gTurtle.x,gTurtle.y,x1,y1);
  gTurtle.x = x1;
  gTurtle.y = y1;
  return distance;
}

// Pivot the Turtle left
int Left(int degrees)
{
  gTurtle.heading += degrees;
  return degrees;
}

// Pivot the Turtle right
int Right(int degrees)
{
  gTurtle.heading -= degrees;
  return degrees;
}

// Marshalling wrappers for our model functions to separate the 
// boiler-plate binding code. For real code use a binding generator.
static int l_Left(lua_State *L)
{
  int n = lua_gettop(L);
  if (n != 1 || !lua_isnumber(L, 1))
  {
    lua_pushstring(L, "expected left degrees");
    lua_error(L);
  }
  int result = Left(lua_tointeger(L, 1));
  lua_pushinteger(L, result);
  return 1;
}

static int l_Right(lua_State *L)
{
  int n = lua_gettop(L);
  if (n != 1 || !lua_isnumber(L, 1))
  {
    lua_pushstring(L, "expected right degrees");
    lua_error(L);
  }
  int result = Right(lua_tointeger(L, 1));
  lua_pushinteger(L, result);
  return 1;
}

static int l_Draw(lua_State *L)
{
  int n = lua_gettop(L);
  if (n != 1 || !lua_isnumber(L, 1))
  {
    lua_pushstring(L, "expected distance");
    lua_error(L);
  }
  int result = Draw(lua_tointeger(L, 1));
  lua_pushinteger(L, result);
  return 1;
}

static int l_Forward(lua_State *L)
{
  int n = lua_gettop(L);
  if (n != 1 || !lua_isnumber(L, 1))
  {
    lua_pushstring(L, "expected distance");
    lua_error(L);
  }
  int result = Forward(lua_tointeger(L, 1));
  lua_pushinteger(L, result);
  return 1;
}

static int l_Canvas(lua_State *L)
{
  int n = lua_gettop(L);
  if (n != 2 || !lua_isnumber(L, 1) || !lua_isnumber(L, 2))
  {
    lua_pushstring(L, "expected canvas width and height");
    lua_error(L);
  }
  int w = lua_tointeger(L, 1);
  int h = lua_tointeger(L, 2);
  gCanvas = CanvasPtr(new Canvas<RGB>(w, h));
  return 0;
}

static int l_Origin(lua_State *L)
{
  int n = lua_gettop(L);
  if (n != 3 || !lua_isnumber(L, 1) 
      || !lua_isnumber(L, 2)
      || !lua_isnumber(L, 3))
  {
    lua_pushstring(L, "expected x, y, heading");
    lua_error(L);
  }
  gTurtle.x = lua_tointeger(L, 1);
  gTurtle.y = lua_tointeger(L, 2);
  gTurtle.heading = lua_tointeger(L, 3);
  return 0;
}

static int l_Save(lua_State *L)
{
  int n = lua_gettop(L);
  if (n != 1 && !lua_isstring(L, 1))
  {
    lua_pushstring(L, "expected name");
    lua_error(L);
  }
  string saveAs = lua_tostring(L, 1);
  saveAs += ".tga";
  int result = gCanvas->save(saveAs);
  lua_pushboolean(L, result);
  return 1;
}

// Contrived demonstration of invoking an external Lua function.
bool turtleEvent(lua_State* L, const string& event)
{
  lua_getfield(L, LUA_GLOBALSINDEX, "event");
  lua_pushstring(L, event.c_str());
  lua_call(L, 1, 1); 
  bool result = lua_toboolean(L, 1);
  lua_pop(L, 1);
  return true;
}

int main(int argc, char **argv)
{
  if (argc < 2)
  {
    cerr << "Usage: luaturtle <lua_file>\n" << endl;
    return 1;
  }
  string source = argv[1];
  if (!fileExists(source))
  {
    std::cerr << "'" << source << "' not found." << endl;
    return 1;
  }

  lua_State *L = luaL_newstate();
  luaL_openlibs(L);

  gCanvas = CanvasPtr(new Canvas<RGB>(512, 512));

  lua_register(L, "Origin", l_Origin);
  lua_register(L, "Canvas", l_Canvas);
  lua_register(L, "Left", l_Left);
  lua_register(L, "Right", l_Right);
  lua_register(L, "Draw", l_Draw);
  lua_register(L, "Forward", l_Forward);
  lua_register(L, "Save", l_Save);

  luaL_dofile(L, source.c_str());
 
  turtleEvent(L, "finished");
  return 0;
}
