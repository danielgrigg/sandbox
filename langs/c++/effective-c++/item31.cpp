/*
 *  item31.cpp
 *  effective
 *
 *  Created by Daniel Grigg on 4/09/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#include "item31.h"
#include <algorithm>
#include <iostream>
#include <vector>
#include <iterator>

using namespace std;

int counter()
{
  static int i = 0;
  return i++;
}

void item31()
{
  cout << "\nItem 31 - partial sorts, nth_element\n";
  
  int myInts[10] = {31, 12, 17, 8, 3, 99, 41, 13, 15, 17};
  vector<int> values(myInts, myInts + 10);
  
  ostream_iterator<int> out_it(cout, " ");
  
  cout << "\nOriginal values:";
  copy(values.begin(), values.end(), out_it);
  
  cout << "\nPartial sorted (7):";
  partial_sort(values.begin(), values.begin()+7, values.end());
  copy(values.begin(), values.end(), out_it);
    
  int myInts2[10] = {31, 12, 17, 8, 3, 99, 41, 13, 15, 17};
  values.assign(myInts2, myInts2 + 10);
  cout << "\nOriginal values:";
  copy(values.begin(), values.end(), out_it);
  
  cout << "\nnth_element (5):";
  nth_element(values.begin(), values.begin()+5, values.end());
  copy(values.begin(), values.end(), out_it);
  
  cout << "\nOriginal values:";

  generate(values.rbegin(), values.rend(), counter);
  copy(values.begin(), values.end(), out_it);
  
  cout << "\nnth_element (median):";
  vector<int>::iterator goal = values.begin() + values.size() / 2;
  nth_element(values.begin(), goal, values.end());
  cout << *goal << " [";
  copy(values.begin(), values.end(), out_it);
  cout << "]\n";
}