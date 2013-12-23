#include <iostream>
#include "../shared/gltools.h"	// OpenGL toolkit
#include "../shared/math3d.h"
#include <math.h>
#include <stdlib.h>

using namespace std;

struct Rectangle
{
  float x;
  float y;
  float width;
  float height;
  
  float x1()const { return x - width / 2.0f; }
  float x2()const { return x + width / 2.0f; }  
  float y1()const { return y - height / 2.0f; }
  float y2()const { return y + height / 2.0f; }  

  void clamp(float boundaryX0, float boundaryY0, float boundaryX1, float boundaryY1)
  {
    float halfWidth = width / 2.0f;
    float halfHeight = height / 2.0f;
    x = x2() > boundaryX1 ? boundaryX1 - halfWidth : x;
    x = x1() < boundaryX0 ? boundaryX0 + halfWidth : x;
    y = y2() > boundaryY1 ? boundaryY1 - halfHeight : y;
    y = y1() < boundaryY0 ? boundaryY0 + halfHeight : y;
  }
};

Rectangle g_rect = {0, 0, 50, 50};
float xStep = 1.0f;
float yStep = 1.0f;

GLfloat windowWidth = 100.0f;
GLfloat windowHeight = 100.0f;



void TimerFunction(int value)
{
  
  if (g_rect.x2() >= windowWidth || g_rect.x1() <= -windowWidth)
  {
    xStep = -xStep;
  }
  
  if (g_rect.y2() >= windowHeight || g_rect.y1() <= -windowHeight)
  {
    yStep = -yStep;
  }
  
  g_rect.x += xStep;
  g_rect.y += yStep;
  
  g_rect.clamp(-windowWidth, -windowHeight, windowWidth, windowHeight);
  
  glutPostRedisplay(); 
  glutTimerFunc(33, TimerFunction, 1);
}

// Called to draw scene
void RenderScene(void)
{
	// Clear the window with current clearing color
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glShadeModel(GL_SMOOTH);
	glEnable(GL_NORMALIZE);
  
  glColor3f(1.0f, 1.0f, 1.0f);
 // glRectf(g_rect.x1(), g_rect.y1(), g_rect.x2(), g_rect.y2());
  
  static float rot = 45.0f;
  
  float A[16], B[16];
  memset(A, 0, sizeof(A));
  memset(B, 0, sizeof(B));
  
  glLoadIdentity();
//  glTranslatef(0, 30, 0);

  glRotatef(45, 0, 1, 0);
  glGetFloatv(GL_MODELVIEW_MATRIX, A);
  
  glRotatef(45, 0, 0, 1);
  
  glGetFloatv(GL_MODELVIEW_MATRIX, B);
  
  glRectf(-20, -20, 20, 20);

  
  for (int i = 0; i < 16; i++)
  {
    cout << A[i] << " ";
  }
  cout << endl;  
  for (int j = 0; j < 16; j++)
    cout << B[j] << " ";
  
  cout << endl;
  rot += .5f;
  if (rot >= 360.0f) rot = 0.0f;
  
	// Flush drawing commands
	glutSwapBuffers();
}

// This function does any needed initialization on the rendering
// context. 
void SetupRC()
{
	// Black background
	glClearColor(0.0f, 0.0f, 0.0f, 1.0f );
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
	
	// Prevent a divide by zero, when window is too short
	// (you cant make a window of zero width).
	if(h == 0)
		h = 1;
  
	float aspectRatio = (GLfloat)w / (GLfloat)h; 
  
  if (w <= h)
  {
    windowWidth = 100.0f;
    windowHeight = 100.0f / aspectRatio;
  }
  else
  {
    windowWidth = 100.0f / aspectRatio;
    windowHeight = 100.0f;
  }

  // Set the viewport to be the entire window
  glViewport(0, 0, w, h);
  
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho (-windowWidth, windowWidth, -windowHeight, windowHeight, 1.0, -1.0);
  
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}  

int main (int argc, char * argv[]) 
{
  glutInit(&argc, argv);
  
  // Double buffered, RGBA with depth buffer
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH);
	glutInitWindowSize(600, 600);
	glutCreateWindow("Skelton GLUT app");
  
	glutReshapeFunc(ChangeSize);
	glutKeyboardFunc(KeyPressFunc);
	glutDisplayFunc(RenderScene);
  glutTimerFunc(33,TimerFunction, 1);
	SetupRC();
  
	glutMainLoop();
  return 0;
}
