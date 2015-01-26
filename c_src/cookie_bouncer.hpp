#ifndef __COOKIE_BOUNCER_HPP__
#define __COOKIE_BOUNCER_HPP__

#include <unordered_map>
#include <utility>
#include <mutex>
#include <thread>

namespace adroll {

  template<typename counter>
  class cookie_bouncer {
  public:
    cookie_bouncer(double halflife = 60.0, bool decay_ = true)
      : tau(halflife / std::log(2)), decay(decay_) {}

    cookie_bouncer(const cookie_bouncer&& cb)
      : tau(cb.tau), kv(cb.kv) {}

    double incr(const std::string key) {
      std::lock_guard<std::mutex> lock(mtx);
      auto search = kv.find(key);
      if (search != kv.end()) {
        return (search->second).incr();
      } else {
        kv.emplace(std::piecewise_construct,
                   std::forward_as_tuple(key),
                   std::forward_as_tuple(tau, decay));
        return 1.0;
      }
    }

    double val(const std::string key) {
      auto search = kv.find(key);
      if (search != kv.end()) {
        return (search->second).val();
      } else {
        return 0.0;
      }
    }

  private:
    const double tau;
    const bool decay;
    std::mutex mtx;
    std::unordered_map<std::string, counter> kv;

    cookie_bouncer(const cookie_bouncer&) = delete;
    const cookie_bouncer& operator=(const cookie_bouncer&) = delete;
  };

}

#endif
