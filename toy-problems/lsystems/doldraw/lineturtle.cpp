#include "lineturtle.h"
#include "lineshape.h"
#include <algorithm>
#include <boost/lambda/bind.hpp>
#include <lap/lap.h>
#include <cmath>
#include "common.h"

using namespace boost::lambda;

inline float radians(float angleDegrees) { return M_PI * angleDegrees / 180.0f; }

LineTurtle::LineTurtle(float lineLength):
_advance(lineLength),
_heading(),
_x(),
_y()
{
}


bool LineTurtle::init(const std::string& program)
{
  _actions['F'] = &LineTurtle::draw;
  _actions['f'] = &LineTurtle::forward;
  _actions['+'] = &LineTurtle::left;
  _actions['-'] = &LineTurtle::right;
  _dol = DOL::makeDOL(program);
  if (!_dol) return false;
  std::cout << "DOL: " << *_dol << std::endl;
  reset();
  return true;
}

void LineTurtle::step()
{
//  shared_ptr<Mesh<VertexP> mesh(new Mesh<VertexP>());
  _mesh = shared_ptr<lap::Mesh<lap::VertexP> >(new lap::Mesh<lap::VertexP>() );
  for_each(_state.begin(), _state.end(), bind(&LineTurtle::doAction, this, _1));

  _shape = makeLineShape(_mesh);
  _state = _dol->step(_state);
}
void LineTurtle::reset()
{
  _state = _dol->axiom();
  _x = _y = 0.0f;
  _heading = 0.0f;
  _shape.reset();
}
void LineTurtle::render(QGLShaderProgram* program)
{
if (_shape)  _shape->render(program);
}

void LineTurtle::draw()
{
  _mesh->_vertices.push_back(makeVertex(_x, _y, 0.0));
  _x = _x + _advance * cosf(radians(_heading));
  _y = _y + _advance * sinf(radians(_heading));
  _mesh->_vertices.push_back(makeVertex(_x, _y, 0.0));

}
void LineTurtle::forward()
{
  _x = _x + _advance * cosf(radians(_heading));
  _y = _y + _advance * sinf(radians(_heading));
}
void LineTurtle::left()
{
  _heading += _dol->theta();
}
void LineTurtle::right()
{
  _heading -= _dol->theta();
}
