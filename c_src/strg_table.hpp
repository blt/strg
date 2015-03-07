#ifndef __ADROLL_STRG_TABLE_HPP__
#define __ADROLL_STRG_TABLE_HPP__

#include <unordered_map>
#include <utility>
#include <mutex>
#include <thread>
#include <list>
#include <cmath>

namespace adroll {

  template<typename K, typename counter>
  class strg_table {
  public:
    typedef K key_t;
    typedef counter counter_t;
    typedef std::list<key_t> access_history_t;

    strg_table(double halflife = 60.0, bool decay_ = true, size_t capacity_ = 10000U)
      : tau(halflife / std::log(2)), decay(decay_), capacity(capacity_), mtx(),
        kv(), access_history() {}

    strg_table(const strg_table&& cb)
      : tau(cb.tau), decay(cb.decay), capacity(cb.capacity), mtx(cb.mtx),
        kv(cb.kv), access_history(cb.access_history) {}

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

    long unsigned int size() {
      return kv.size();
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

    strg_table(const strg_table&) = delete;
    const strg_table& operator=(const strg_table&) = delete;
  };

}

#endif
