#ifndef MYLIB_H
#define MYLIB_H

#include <string>
#include <iostream>

using namespace std;

class MyLib
{
  public:
    MyLib(const string &who);

    void hello()const
    {
      cout << "Hello " << m_who << endl;
    }
  private:
    string m_who;
};

#endif
