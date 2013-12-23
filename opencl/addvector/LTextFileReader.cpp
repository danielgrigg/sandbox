/*!
 *  \file LTextFileReader.cpp
 *  \brief Contains the implementation of LTextFileReader.
 *  \author Daniel Grigg
 *  \date 2011/02/12
 *  Lexi
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 */

#include "LTextFileReader.h"
#include <fstream>
#include <iostream>

bool LTextFileReader::read(const std::string &path, std::string &contents)
{
  std::ifstream ifs(path.c_str(), std::fstream::binary);
  if (!ifs.is_open()) return false;
  ifs.seekg (0, std::ios::end);
  uint64_t length = ifs.tellg();
  ifs.seekg (0, std::ios::beg);
  char *buffer = new char [length];
  ifs.read (buffer,length);
  ifs.close();

  contents.assign(buffer, length);
  delete[] buffer;
  return true;
}

