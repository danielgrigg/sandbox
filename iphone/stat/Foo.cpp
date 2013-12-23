//
//  Foo.cpp
//  stat
//
//  Created by Daniel Grigg on 24/01/11.
//  Copyright (c) 2011 Daniel Grigg. All rights reserved.
//

#include "Foo.h"

Foo::Foo(int x)
{
  m_x = x;
}

int Foo::bar()
{ 
  return m_x;
}
