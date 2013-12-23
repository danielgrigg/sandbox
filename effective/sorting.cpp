/*
 *  sorting.cpp
 *  effective
 *
 *  Created by Daniel Grigg on 6/09/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

//#define DRIVE_SELF

#ifndef DRIVE_SELF
#include "sorting.h"
#endif

//#include <ctime>
//#include <cstdlib>

#include <vector>
#include <iostream>
#include <algorithm>
#include <iterator>

using namespace std;

struct UniqueNumber {
  int current;
  UniqueNumber() {current=0;}
  int operator()() {return ++current;}
} UniqueNumberInstance;

struct MyStdSorter
{
  bool operator()(int i, int j) { return i < j; }
} MyStdSorterInstance;

int myQSorter(const void *a, const void *b)
{
  return *(int*)a - *(int*)b;
}

void sorting()
{
  vector<int> values(50000000);

  generate(values.begin(), values.end(), UniqueNumberInstance);
  random_shuffle(values.begin(), values.end());

  time_t start, end;
  time(&start);
  qsort(&values[0], values.size(), sizeof(int), myQSorter);
  time(&end);
  
//  ostream_iterator<int> out_it(cout, " ");  
//  cout << "Values: ";
//  copy(values.begin(), values.end(), out_it);
//  cout << "\n";
  cout << "QSort took " << difftime(end, start) << " (s) to sort " << values.size() << " records.\n";

  random_shuffle(values.begin(), values.end());
  time(&start);
  std::sort(values.begin(), values.end(), MyStdSorterInstance);
  time(&end);
  cout << "Std Sort took " << difftime(end, start) << " (s) to sort " << values.size() << " records.\n";
}

#ifdef DRIVE_SELF
int main()
{
  sorting();
  return 0;
}
#endif

