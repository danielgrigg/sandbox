/*
 *  ShowMe.h
 *  effective
 *
 *  Created by Daniel Grigg on 3/09/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */
#if !defined SHOW_ME_H
#define SHOW_ME_H

#include <iostream>

using std::cout;

class ShowMe
{
public:
  
  ShowMe():
    m_value(0)
  {
    cout << "ShowMe default constructor\n";
  }
  
  ShowMe(int v):
  m_value(v)
  {
    cout << "ShowMe :int constructor\n";
  }
  
  ShowMe(const ShowMe &rhs):
  m_value(rhs.m_value) 
  { 
    cout << "ShowMe copy constructor\n"; 
  }
  
  ~ShowMe() 
  { 
    cout << "ShowMe destructor\n"; 
  }
  
  ShowMe & operator=(const ShowMe &rhs) 
  {
    m_value = rhs.m_value;
    cout << "ShowMe assignment\n"; return *this; 
  }  
  
private:
  
  int m_value;
};

#endif