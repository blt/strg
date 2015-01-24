#ifndef __DECAYING_COUNTER_HPP__
#define __DECAYING_COUNTER_HPP__

#include <cmath>
#include <ctime>
#include <mutex>
#include <thread>

namespace adroll {

  class decaying_counter {
  public:
    decaying_counter(const double tau_ = 0.01,
                     const double value_ = 0.0,
                     const std::time_t updated_time_ = std::time(nullptr))
      : value(value_), tau(tau_), updated_time(updated_time_) {}

    decaying_counter(const decaying_counter&& dc)
      : value(dc.value), tau(dc.tau), updated_time(dc.updated_time) {}

    double incr() {
      std::lock_guard<std::mutex> lock(mtx);
      update();
      value++;
      return value;
    }

    double val() {
      std::lock_guard<std::mutex> lock(mtx);
      return update();
    }

  private:
    double value;
    const double tau;
    std::time_t updated_time;
    std::mutex mtx;

    decaying_counter(const decaying_counter&) = delete;
    const decaying_counter& operator=(const decaying_counter&) = delete;

    double update() {
      const std::time_t now = std::time(nullptr);
      const double now_diff = std::difftime(now, updated_time);

      if (now_diff > 0) {
        value *= std::exp(now_diff * (-1.0 / tau));
      }

      updated_time = now;
      return value;
    }
  };

}

#endif
