//
//  main.cpp
//  eigendemo
//
//  Created by Daniel Grigg on 1/04/2014.
//  Copyright (c) 2014 Daniel Grigg. All rights reserved.
//

#include <iostream>
#include <algorithm>
#include <Eigen/Eigen>
#include <Eigen/OpenGLSupport>

using namespace Eigen;
int main() {
  float rawA[16];
  
  Matrix4f A = Matrix4f::Random();
  std::cout << "A:\n" << A << std::endl;
  

  // Stored column-major weee
  std::copy(A.data(), A.data() + 16, rawA);
  rawA[1] = 1234.1234;
  Map<Matrix4f> M(rawA, 4, 4);
  std::cout << "M:\n" << M << std::endl;
  return 0;
}