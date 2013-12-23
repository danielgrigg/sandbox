/*!
 *  \file LTextFileReader.hpp
 *  \brief Interface for LTextFileReader.
 *  \author Daniel Grigg
 *  \date 2011/02/12
 *  Lexi
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 */

#ifndef LEXI_IO_TEXTFILEREADER_HPP
#define LEXI_IO_TEXTFILEREADER_HPP

#include <string>

//! \brief LTextFileReader
class LTextFileReader
{
public:
  static bool read(const std::string &path, std::string &contents);
private:
};

#endif // ifndef LEXI_IO_TEXTFILEREADER_HPP
