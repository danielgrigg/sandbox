#include <iostream>
#include <sstream>
#include <string>
#include <cstdio>
#include <dispatch/dispatch.h>

using namespace std;

void post_log(void* context) {
  std::string* log = (string*)context;
  cout << *log << endl;
}

void log(const string& s, const dispatch_queue_t& q) {
  dispatch_sync_f(q, (void*)&s, post_log);
}

int g_ndone = 0;

int main(int argc, char **argv)
{
  string data_path = "/tmp/dispatch_io_data/";
  if (argc > 2) {
    data_path = argv[1];
  }

  // Main queue for asynchronous operations
  dispatch_queue_t main_queue = 
    dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0);

  // Queue for logging messages to stdout
  dispatch_queue_t log_queue = 
    dispatch_queue_create("com.danielgrigg.dispatch.log", DISPATCH_QUEUE_SERIAL);

  // A queue to serialise access to our global variables.
  dispatch_queue_t message_queue = 
    dispatch_queue_create("com.danielgrigg.dispatch.message", DISPATCH_QUEUE_SERIAL);

  // Condition variable for completing all reads
  dispatch_semaphore_t all_reads_done = dispatch_semaphore_create(0);

  const int N = 30;

  // Process all N files simultaneously.  Of course this means
  // N files will be stored in memory, which for our test-data
  // is a 'quite large'.  You should batch over fractions of N
  // depending on how much memory you have to play with.
  for (int i = 0; i < N; ++i) {
    std::ostringstream datum_stream;
    datum_stream << data_path << "a" << i;

    // Open the file, then get its length and file-descriptor.
    const std::string datum = datum_stream.str();
    FILE* fs = fopen(datum.c_str(), "r");
    if (fs == NULL) {
      cout << "Unable to open " << datum << endl;
      continue;
    }
      
    fseek(fs, 0, SEEK_END);
    int flength = ftell(fs);
    rewind(fs);
    int fd = fileno(fs);

    log("reading " + datum, log_queue);
    dispatch_read(fd, flength, main_queue, 
        ^(dispatch_data_t data, int error){

        // Here's where you'd normally do something 
        // with the data...but we just cleanup here.
        log("finished " + datum, log_queue);
        fclose(fs);
        dispatch_sync(message_queue, ^{
          if (++g_ndone == N) {
            dispatch_semaphore_signal(all_reads_done);
          }
          });
        });
  }
  dispatch_semaphore_wait(all_reads_done,  DISPATCH_TIME_FOREVER);
  log("Finished!", log_queue);
  return 0;
}
