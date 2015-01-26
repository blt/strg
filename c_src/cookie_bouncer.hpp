#ifndef __COOKIE_BOUNCER_HPP__
#define __COOKIE_BOUNCER_HPP__

#include <unordered_map>
#include <utility>
#include <mutex>
#include <thread>
#include <list>

namespace adroll {

  template<typename K, typename counter>
  class cookie_bouncer {
  public:
    typedef K key_t;
    typedef counter counter_t;
    typedef std::list<key_t> access_history_t;

    cookie_bouncer(double halflife = 60.0, bool decay_ = true, size_t capacity_ = 10000U)
      : tau(halflife / std::log(2)), decay(decay_), capacity(capacity_) {}

    cookie_bouncer(const cookie_bouncer&& cb)
      : tau(cb.tau), kv(cb.kv) {}

    double incr(const std::string key) {
      std::lock_guard<std::mutex> lock(mtx);
      auto search = kv.find(key);
      if (search != kv.end()) {
        access_history.splice(access_history.end(), access_history, (search->second).second);
        return (search->second).first.incr();
      } else {
        if (kv.size() == capacity) {
          evict();
        }
        auto access_it = access_history.insert(access_history.end(), key);
        auto value =  std::make_pair(counter {tau, decay}, access_it);
        kv.insert(std::make_pair(key, std::move(value)));
        return 1.0;
      }
    }

    double val(const std::string key) {
      auto search = kv.find(key);
      if (search != kv.end()) {
        return (search->second).first.val();
      } else {
        return 0.0;
      }
    }

  private:
    const double tau;
    const bool decay;
    const size_t capacity;
    std::mutex mtx;
    std::unordered_map<key_t, std::pair<counter_t, typename access_history_t::iterator>> kv;
    access_history_t access_history;

    void evict() {
      auto it = kv.find(access_history.front());
      kv.erase(it);
      access_history.pop_front();
    }

    cookie_bouncer(const cookie_bouncer&) = delete;
    const cookie_bouncer& operator=(const cookie_bouncer&) = delete;
  };

}

#endif
