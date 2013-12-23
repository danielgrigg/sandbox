//
//  main.m
//  pointview
//
//  Created by Daniel Grigg on 6/02/11.
//  Copyright (c) 2011 Daniel Grigg. All rights reserved.
//

#import <UIKit/UIKit.h>
#include <lexi/Core/LMath.hpp>
#include <lexi/Geometry/LAABB.h>
#include <iostream>

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

int main(int argc, char *argv[]) {
  NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
  
  testAABB();
  
  int retVal = UIApplicationMain(argc, argv, nil, nil);
  [pool release];
  return retVal;
}
