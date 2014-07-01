/*!
 *  \file LTextFileReader.cpp
 *  \brief Contains the implementation of LTextFileReader.
 *  \author Daniel Grigg
 *  \date 2011/02/12
 *  Lexi
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 */

#include "textfile.h"
#include <fstream>
#include <iostream>

using namespace std;

bool TextFileReader::read(const string &path, string &contents)
{
    ifstream ifs(path.c_str(), fstream::binary);

    ifs.seekg (0, ios::end);
    uint64_t length = ifs.tellg();
    ifs.seekg (0, ios::beg);
    char *buffer = new char [length];
    ifs.read (buffer,length);
    ifs.close();
    
    contents.assign(buffer, length);
    delete[] buffer;
    return true;
}

