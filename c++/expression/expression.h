#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <iostream>

class Order {
};

class Contract {
};

template <typename P>
void invoice(const P& p);

void invoice(const Order& order) {
  std::cout << "invoice for Order" << std::endl;
}

void invoice(const Contract& contract) {
  std::cout << "invoice for Contract" << std::endl;
}


#endif

