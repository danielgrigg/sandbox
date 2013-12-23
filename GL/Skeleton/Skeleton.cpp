#include <iostream>
#include "../shared/gltools.h"	// OpenGL toolkit
#include "../shared/math3d.h"
#include <math.h>
#include <stdlib.h>

// Called to draw scene
void RenderScene(void)
{
	// Clear the window with current clearing color
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glShadeModel(GL_SMOOTH);
	glEnable(GL_NORMALIZE);

	// Flush drawing commands
	glutSwapBuffers();
}

// This function does any needed initialization on the rendering
// context. 
void SetupRC()
{
	// Black background
	glClearColor(1.0f, 0.0f, 0.0f, 1.0f );
}

void KeyPressFunc(unsigned char key, int x, int y)
{
	if(key == 32)
  {
  }
  
	// Refresh the Window
	glutPostRedisplay();
}

  
void ChangeSize(int w, int h)
{
  
	// Calculate new clipping volume
	GLfloat windowWidth;
	GLfloat windowHeight;
  
	// Prevent a divide by zero, when window is too short
	// (you cant make a window of zero width).
	if(h == 0)
		h = 1;
  
	// Keep the square square
	if (w <= h) 
  {
		windowHeight = 100.0f*(GLfloat)h/(GLfloat)w;
		windowWidth = 100.0f;
  }
  else 
  {
		windowWidth = 100.0f*(GLfloat)w/(GLfloat)h;
		windowHeight = 100.0f;
  }
  
  // Set the viewport to be the entire window
  glViewport(0, 0, w, h);
  
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  
  // Set the clipping volume
  glOrtho(-100.0f, windowWidth, -100.0f, windowHeight, -200.0f, 200.0f);
  
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}  

int main (int argc, char * argv[]) 
{
  glutInit(&argc, argv);
  
  // Double buffered, RGBA with depth buffer
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH);
	glutInitWindowSize(800, 600);
	glutCreateWindow("Skelton GLUT app");
  
	glutReshapeFunc(ChangeSize);
	glutKeyboardFunc(KeyPressFunc);
	glutDisplayFunc(RenderScene);
  
	SetupRC();
  
	glutMainLoop();
  return 0;
}
