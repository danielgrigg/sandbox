/*
 *  item24.cpp
 *  effective
 *
 *  Created by Daniel Grigg on 3/09/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#include "item24.h"

#include <iostream>
#include <map>

#include "ShowMe.h"

using namespace std;


void item24()
{
  map<int, ShowMe> m;
  
  cout << "Indexer insertion\n";
  m[1] = 2;
  
  cout << "insert insertion\n";
  m.insert(map<int,ShowMe>::value_type(2, 3));
//  m[2] = 3;
//  m[3] = 4;
}