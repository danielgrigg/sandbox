#include <mach/mach_time.h>
#include <stdint.h>    
#include <iostream>
using namespace std;

// Utility function for elapsed time in seconds.  Example only as you'll lose some precision
// working in seconds instead of nanoseconds...
double elapsed_us(uint64_t start_mtu, uint64_t end_mtu) 
{
    uint64_t delta_mtu = end_mtu - start_mtu;
    static double mtusPerUSec = 0.0;
    if (0.0 == mtusPerUSec) 
    {
        mach_timebase_info_data_t info;
        if (mach_timebase_info(&info)) 
        {
          // Oh noes!
        }
        mtusPerUSec = 1E-3 * (double)info.numer / (double)info.denom;
    }
    return delta_mtu * mtusPerUSec;
}


int main()
{
  char x[255];
  cout << "Press enter to start timing!" << endl;
  cin.getline(x,255);
  
  uint64_t startTime = mach_absolute_time( );
  cout << "Started timing..." << endl;
  cout << "Press enter to stop timing." << endl;
  cin.getline(x,255);

  uint64_t endTime = mach_absolute_time( );
  double interval = elapsed_us(startTime, endTime);
  cout << "Stopped timing after: " << interval * 1e-6 << " s." << endl; 
  return 0;
}

