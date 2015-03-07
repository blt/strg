#ifndef __ADROLL_COUNTER_HPP__
#define __ADROLL_COUNTER_HPP__

#include <cmath>
#include <ctime>
#include <mutex>
#include <thread>

namespace adroll {

  class counter {
  public:
    counter(const double tau_ = 0.01,
            const bool decay_ = true,
            const double value_ = 1.0,
            const std::time_t updated_time_ = std::time(nullptr))
      : value(value_), decay(decay_), tau(tau_), updated_time(updated_time_), mtx() {}

    counter(const counter&& dc)
      : value(dc.value), decay(dc.decay), tau(dc.tau), updated_time(dc.updated_time), mtx() {}

    double incr() {
      std::lock_guard<std::mutex> lock(mtx);
      update();
      value++;
      return value;
    }

    double val() {
      std::lock_guard<std::mutex> lock(mtx);
      update();
      return value;
    }

  private:
    double value;
    const bool decay;
    const double tau;
    std::time_t updated_time;
    std::mutex mtx;

    counter(const counter&) = delete;
    const counter& operator=(const counter&) = delete;

    void update() {
      // TODO This branch can be removed when I figure out how to do polymorphic
      // collections in Modern C++. There would be, instead, an
      // 'increasing_counter' class that will not do any decay. The motivation
      // here is to move on down the road. There's some wasted space and a
      // branch misprediction. Less lolspeed.
      if (decay) {
        const std::time_t now = std::time(nullptr);
        const double now_diff = std::difftime(now, updated_time);

        if (now_diff > 0) {
          value *= std::exp(now_diff * (-1.0 / tau));
        }

        updated_time = now;
      }
    }
  };

}

#endif
