class Entity {};
class Dragline : public Entity
{};

template <typename E>
struct State
{
  State(const char* name):_name(name){}

  const char* _name;
};

typedef Dragline* DraglinePtr;

#define DECLARE_STATE(name,entity) \
  class name : public State<entity> \
{\
    public:\
           name():State<entity>(#name){}\
      void doEnter(entity##Ptr e);\
      void doExecute(entity##Ptr e, long simTimeMillis);\
      void doExit(entity##Ptr e);\
}



DECLARE_STATE(Spinning, Dragline);

int main()
{

  return 0;
}
