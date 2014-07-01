#include <boost/bind.hpp>
#include <iostream>
#include <string>

std::string f(std::string const & x)
{
    return "f(" + x + ")";
}

std::string g(std::string const & x)
{
    return "g(" + x + ")";
}

std::string h(std::string const & x, std::string const & y)
{
    return "h(" + x + ", " + y + ")";
}

std::string k()
{
    return "k()";
}

template<class F> void test(F f)
{
    std::cout << f("x", "y") << '\n';
}

int main()
{
    using namespace boost;

    // compose_f_gx

    test( bind(f, bind(g, _1)) );

    // compose_f_hxy

    test( bind(f, bind(h, _1, _2)) );

    // compose_h_fx_gx

    test( bind(h, bind(f, _1), bind(g, _1)) );

    // compose_h_fx_gy

    test( bind(h, bind(f, _1), bind(g, _2)) );

    // compose_f_k

    test( bind(f, bind(k)) );

    return 0;
}

