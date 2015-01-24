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
    cookie_bouncer(double halflife = 60.0) : tau(halflife / std::log(2)) {}

    void incr(const std::string key) {
      std::lock_guard<std::mutex> lock(mtx);
      auto search = kv.find(key);
      if (search != kv.end()) {
        (search->second).incr();
      } else {
        auto dc = counter{ tau };
        kv.emplace(std::make_pair(key, std::move(dc)));
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
    std::mutex mtx;
    std::unordered_map<std::string, counter> kv;

    cookie_bouncer(const cookie_bouncer&) = delete;
    const cookie_bouncer& operator=(const cookie_bouncer&) = delete;
  };

}

#endif
