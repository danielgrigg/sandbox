#include <string>
#include <vector>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/fstream.hpp>    // ditto
#include <boost/filesystem/convenience.hpp>
#include <iostream>                        // for std::cout

using namespace boost::filesystem;  
using namespace std;

// pulled from boost filesystem homepage
bool find_file( const path & dir_path,     // in this directory,
                const std::string & file_name, // search for this name,
                path & path_found )        // placing path here if found
{
  if ( !exists( dir_path ) ) return false;
  directory_iterator end_itr; // default construction yields past-the-end
  for ( directory_iterator itr( dir_path );
        itr != end_itr;
        ++itr )
  {
    if ( is_directory( *itr ) )
    {
      if ( find_file( *itr, file_name, path_found ) ) return true;
    }
    else if ( itr->leaf() == file_name ) // see below
    {
      path_found = *itr;
      return true;
    }
  }
  return false;
}

bool filesInDir(const path & dirPath, std::vector<path> &filePaths)
{
  if (!exists(dirPath)) return false;

  directory_iterator endIter;
  for (directory_iterator iter( dirPath ); iter != endIter; ++iter )
  {
    filePaths.push_back(*iter);
  }
  return true;

}

int main(int argc, char **argv)
{
  path foundAt;
  path thisDir( "." );

  if (find_file(thisDir, "filesystem.cpp", foundAt))
  {
    cout << "Found " << foundAt << " with basename " << basename(foundAt) << endl;
  }

  if (find_file("..", "bar.txt", foundAt))
  {
    cout << "Found " << foundAt << " with extension " << extension(foundAt) <<  endl;
  }

  vector<path> paths;
  filesInDir(".", paths);
  ostream_iterator<path> osi(cout, "\n");
  copy(paths.begin(), paths.end(), osi);
  return 0;
}
