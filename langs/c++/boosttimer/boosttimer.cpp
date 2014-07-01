#include <boost/asio.hpp>
#include <boost/bind.hpp>
#include <boost/enable_shared_from_this.hpp>

#include <iostream>

class Timer : public boost::enable_shared_from_this<Timer>
{
  public:
    Timer( boost::asio::io_service& io_service) :
      _io_service( io_service ),
      _timer( io_service )
  { }

    void start() {
      _timer.expires_from_now( boost::posix_time::seconds( 0 ));
      _timer.async_wait( boost::bind( &Timer::handler, shared_from_this(), boost::asio::placeholders::error));
    }

  private:
    void handler( const boost::system::error_code& error)
    {
      if ( error ) {
        std::cerr << error.message() << std::endl;
        return;
      }

      std::cout << "handler" << std::endl;
      _timer.expires_from_now( boost::posix_time::seconds( 1 ));
      _timer.async_wait(
          boost::bind( &Timer::handler, shared_from_this(), boost::asio::placeholders::error));
    }

  private:
    boost::asio::io_service& _io_service;
    boost::asio::deadline_timer _timer;
};

  int main()
{
  boost::asio::io_service io_service;
  boost::shared_ptr<Timer> timer( new Timer( io_service ));
  timer->start();
  io_service.run();
}

