#include <iostream>

#include "cookie_bouncer.hpp"
#include "decaying_counter.hpp"

#include <chrono>
#include <thread>

int main() {
  adroll::cookie_bouncer<adroll::decaying_counter> cb;

  double v;
  cb.incr("asdf");
  cb.incr("asdf");
  cb.incr("asdf");
  cb.incr("asdf");
  cb.incr("asdf");
  while ((v = cb.val("asdf")) > 0.05) {
    std::cout << cb.val("asdf") << std::endl;

    std::this_thread::sleep_for(std::chrono::seconds(1U));
  }
}
