
// simple example based on Ciaran's tute
#include "SDL.h"

#include <time.h>
#include <OpenGLES/ES2/gl.h>
#include <OpenGLES/ES2/glext.h>
#include <memory>


float random_float() {
  return random() / (float)((2u << 31) - 1);
}


class Graphics {
public:
  Graphics(SDL_Window* window):
  _window(window) {
    _gl = SDL_GL_CreateContext(window);
  }
  
  void render() {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glClearColor(random_float(), random_float(), random_float(), 1.0f);
    
    
    SDL_GL_SwapWindow(_window);
  }
  ~Graphics() {
    SDL_GL_DeleteContext(_gl);
  }
private:
  SDL_Window* _window;
  SDL_GLContext _gl;
};

int main(int argc, char* argv[]) {
  
  if (SDL_Init(SDL_INIT_VIDEO) < 0) {
    printf("Error initialising SDL\n");
    return 1;
  }
  
  SDL_DisplayMode displayMode;
  SDL_GetDesktopDisplayMode(0, &displayMode);
  
  auto window =
  SDL_CreateWindow(NULL,
                   0,
                   0,
                   displayMode.w,
                   displayMode.h,
                   SDL_WINDOW_OPENGL | SDL_WINDOW_FULLSCREEN);
  if (!window) {
    printf("Error creating window\n");
    return 1;
  }
  
  auto graphics = std::unique_ptr<Graphics>(new Graphics(window));
  
  auto callback = [](void* param) {
    auto g = reinterpret_cast<Graphics*>(param);
    g->render();
  };
  
  SDL_iPhoneSetAnimationCallback(window, 12, callback, graphics.get());
  SDL_Event event;
  auto done = false;
  
  while (!done) {
    SDL_PumpEvents();
    while (SDL_PollEvent(&event)) {
      switch (event.type) {
        case SDL_QUIT:
          done = true;
          break;
        case SDL_APP_DIDENTERFOREGROUND:
          SDL_Log("SDL_APP_DIDENTERFOREGROUND");
          break;
        case SDL_APP_DIDENTERBACKGROUND:
          SDL_Log("SDL_APP_DIDENTERBACKGROUND");
          break;
        case SDL_APP_WILLENTERFOREGROUND:
          SDL_Log("SDL_APP_WILLENTERFOREGROUND");
          break;
        case SDL_APP_WILLENTERBACKGROUND:
          SDL_Log("SDL_APP_WILLENTERBACKGROUND");
          break;
        case SDL_APP_LOWMEMORY:
          SDL_Log("SDL_APP_LOWMEMORY");
          break;
        case SDL_APP_TERMINATING:
          SDL_Log("SDL_APP_TERMINATING");
          break;
        case SDL_FINGERMOTION:
          SDL_Log("SDL_FINGERMOTION");
          break;
        case SDL_FINGERDOWN:
          SDL_Log("SDL_FINGERDOWN");
          break;
          
        default:
          break;
      }
    }
    //graphics->render();
    
  }
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
}