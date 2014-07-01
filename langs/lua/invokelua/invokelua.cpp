#include <iostream>
#include <fstream>
#include <lua.hpp>
//#include <lualib.h>
//k#include <lauxlib.h> // Higher-level functions

using namespace std;

bool fileExists(const std::string &path)                                                   
{
  std::ifstream ifs(path.c_str());
  return ifs.is_open();
}

int main(int argc, char **argv)
{
  if (!fileExists("invokelua.lua"))
  {
    std::cerr << "invokelua.lua not found" << endl;
    return 1;
  }

  // Context for all lua funcs
  lua_State *L = luaL_newstate();
  luaL_openlibs(L);
  luaL_dofile(L, "invokelua.lua");
  lua_getfield(L, LUA_GLOBALSINDEX, "foo");
//  lua_pushinteger(L, 
  lua_call(L, 0, 1);  // Call 'foo', 0 args, 1 result
  int result = lua_tointeger(L, 1);
  lua_pop(L, 1);

  std::cout << "foo(" << ") returned " << result << endl;

  int top = lua_gettop(L);
  std::cerr << "top = " << top << endl;
  return 0;
}

