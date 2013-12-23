//
//  main.m
//  pointview
//
//  Created by Daniel Grigg on 6/02/11.
//  Copyright 2011 Daniel Grigg. All rights reserved.
//

#import <UIKit/UIKit.h>
#include <iostream>

#if 1
#include <lexi/Core/LMath.hpp>
#include <lexi/Geometry/LAABB.h>

void testAABB()
{
  LAABB box;
  box.addPoint(LPoint(-1,-1,-1));
  box.addPoint(LPoint(1,1,1));
  box.addPoint(LPoint(0,0,0));
  
  std::cout << "BBox: " << box << std::endl;
  if (box.contains(LPoint(0,.8, .8))) std::cout << "contains (0,.8,.8)" << std::endl;
  else std::cout << "does not contain (0,.8,.8)" << std::endl;
  }
#endif
 
int main(int argc, char *argv[]) 
{
  NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
  int retVal = UIApplicationMain(argc, argv, nil, nil);
  [pool release];
  return retVal;
}
