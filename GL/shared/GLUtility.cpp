#include "GLUtility.h"
#include "glforward.h"
#include <iostream>
#include <map>

//TODO - portable backtracing
#include <execinfo.h>

bool checkGLErrors()
{
  static std::map<GLenum, const char*> codes;
  static GLenum lastError = GL_NO_ERROR;
  if (codes.empty())
  {
    codes[GL_NO_ERROR] = "OK";
    codes[GL_INVALID_ENUM] = "Invalid enum";
    codes[GL_INVALID_VALUE] = "Argument out-of-range";
    codes[GL_INVALID_OPERATION] = "Operation not-allowed";
    codes[GL_STACK_OVERFLOW] = "Stack overflow";
    codes[GL_STACK_UNDERFLOW] = "Stack underflow";
    codes[GL_OUT_OF_MEMORY] = "Out of memory";
    codes[GL_TABLE_TOO_LARGE] = "Table too large";
  }
  GLenum e = glGetError();
  if (e != GL_NO_ERROR && lastError != e)
  {
    //TODO - portable backtracing
    void* callstack[256];
    int i, frames = backtrace(callstack, 256);
    char** strs = backtrace_symbols(callstack, frames);
    for (i = 0; i < frames; ++i) { printf("%s\n", strs[i]); }
    free(strs);

    std::cerr << "GL error: " << codes[e] << "\n";
    lastError = e;
    return false;
  }
  return true;
}


