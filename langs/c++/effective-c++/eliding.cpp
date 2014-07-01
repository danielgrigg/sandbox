/*
 *  eliding.cpp
 *  effective
 *
 *  Created by Daniel Grigg on 11/09/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#include "eliding.h"

#include <iostream>

struct C
{
  C() 
  {
    std::cout << "Inside C constructor.\n";
  }
  
  C(const C&) { std::cout << "A copy was made.\n"; }
  
  ~C()
  {
    std::cout << "Inside C destructor.\n";
  }
};

C f()
{
  return C();
}
		
void eliding()
{
  std::cout << "Hello World!\n";
  C obj = f();
  
 // C bob = obj;
}