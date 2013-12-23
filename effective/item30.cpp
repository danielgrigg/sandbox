/*
 *  item30.cpp
 *  effective
 *
 *  Created by Daniel Grigg on 4/09/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#include "item30.h"
#include <algorithm>
#include <iostream>
#include <vector>
#include <iterator>

using namespace std;

int transmogrify(int x)
{
  return 2 * x;
}

void item30()
{
  cout << "Item 30:\n";
  
  vector<int> values(10);
  for (int i =0; i < 10; ++i)
  {
    values[i] = i;
  }
  
  vector<int> results;
  results.reserve(results.size() + values.size());
  
  transform(values.begin(), values.end(), back_inserter(results), transmogrify);
  
  ostream_iterator<int> out_it (cout, ", ");
  copy(results.begin(), results.end(), out_it);
  
  
}