#ifndef LINETURTLE_H
#define LINETURTLE_H

#include <string>
#include <tr1/unordered_map>
#include <boost/function.hpp>
#include <tr1/memory>
#include <QGLShaderProgram>
#include <lap/lap.h>
#include "dol.h"
#include "lineshape.h"

using namespace std::tr1;

class LineTurtle;
typedef boost::function<void(LineTurtle*)> Action;
typedef unordered_map<char, Action > ActionMap;
typedef shared_ptr<LineTurtle> LineTurtlePtr;

class LineTurtle
{
public:
    LineTurtle( float lineLength);

    bool init(const std::string& program);

    static LineTurtlePtr makeLineTurtle(const std::string& program,
                                        float lineLength)
    {
      LineTurtlePtr turtle(new LineTurtle(lineLength));
      if (!turtle->init(program)) return LineTurtlePtr();
      return turtle;
    }
    void step();
    void reset();
    void render(QGLShaderProgram* program);
    lap::BoundingBox<lap::float3> bounds()const
    {
      if (_shape) return _shape->bounds();
      return lap::BoundingBox<lap::float3>();
    }

private:
    void draw();
    void forward();
    void left();
    void right();
    void doAction(const char a) { _actions[a](this); }


    float _advance;
    ActionMap _actions;
    DOLPtr _dol;
    std::string _state;
    float _heading;
    float _x;
    float _y;
    LineShapePtr _shape;
    shared_ptr<lap::Mesh<lap::VertexP> > _mesh;
};

#endif // LINETURTLE_H
