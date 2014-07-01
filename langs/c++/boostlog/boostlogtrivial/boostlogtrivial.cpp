#include <boost/log/core.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/filters.hpp>
#include <boost/log/formatters.hpp>
#include <boost/log/sinks.hpp>
#include <boost/log/attributes.hpp>
#include <boost/log/utility/init/to_file.hpp>

namespace logging = boost::log;
namespace sinks = boost::log::sinks;
namespace src = boost::log::sources;
namespace fmt = boost::log::formatters;
namespace flt = boost::log::filters;
namespace attrs = boost::log::attributes;
namespace keywords = boost::log::keywords;

/*
void init()
{
    logging::init_log_to_file
    (
        keywords::file_name = "sample_%N.log",
        keywords::rotation_size = 10 * 1024 * 1024,
        keywords::time_based_rotation = sinks::file::rotation_at_time_point(0, 0, 0),
        keywords::format = "[%TimeStamp%]: %_%"
    );

    logging::core::get()->set_filter
    (
        flt::attr< logging::trivial::severity_level >("Severity") >= logging::trivial::info
    );
}
*/ 

void init()
{
    
#if 1
    logging::init_log_to_file("error.log");
    logging::core::get()->set_filter
      (
       flt::attr< logging::trivial::severity_level >("Error") >= logging::trivial::error
      );
#else 
logging::core::get()->set_filter
    (
        flt::attr< logging::trivial::severity_level >("Severity") >= logging::trivial::warning
    );

#endif
}

int main(int, char*[])
{
  init();

  BOOST_LOG_TRIVIAL(trace) << "A trace severity message";
  BOOST_LOG_TRIVIAL(debug) << "A debug severity message";
  BOOST_LOG_TRIVIAL(info) << "An informational severity message";
  BOOST_LOG_TRIVIAL(warning) << "A warning severity message";
  BOOST_LOG_TRIVIAL(error) << "An error severity message";
  BOOST_LOG_TRIVIAL(fatal) << "A fatal severity message";
}

