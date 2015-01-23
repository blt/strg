#include <iostream>
#include <cstdint>
#include <cmath>
#include <ctime>

#include <chrono>
#include <thread>
#include <utility>

#include <unordered_map>

class decaying_counter {
public:
  decaying_counter(const double tau_ = 0.01,
                   const double value_ = 0.0,
                   const std::time_t updated_time_ = std::time(nullptr))
    : value(value_), tau(tau_), updated_time(updated_time_) {}

  decaying_counter(const decaying_counter&& dc)
    : value(dc.value), tau(dc.tau), updated_time(dc.updated_time) {}

  double incr() {
    update();
    value++;
    return value;
  }

  double val() {
    return update();
  }

  double update() {
    const std::time_t now = std::time(nullptr);
    const double now_diff = std::difftime(now, updated_time);

    if (now_diff > 0) {
      value *= std::exp(now_diff * (-1.0 / tau));
    }

    updated_time = now;
    return value;
  }

private:
  double value;
  const double tau;
  std::time_t updated_time;

  decaying_counter(const decaying_counter&) = delete;
  const decaying_counter& operator=(const decaying_counter&) = delete;
};

class cookie_bouncer {
public:
  cookie_bouncer(double halflife = 1.0) : tau(halflife / std::log(2)) {}

  void incr(const std::string key) {
    auto search = kv.find(key);
    if (search != kv.end()) {
      (search->second).incr();
    } else {
      auto dc = decaying_counter{ tau };
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
  std::unordered_map<std::string, decaying_counter> kv;

  cookie_bouncer(const cookie_bouncer&) = delete;
  const cookie_bouncer& operator=(const cookie_bouncer&) = delete;
};

int main() {
  cookie_bouncer cb;

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
