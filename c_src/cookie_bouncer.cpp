#include <iostream>

#include "cookie_bouncer.hpp"
#include "decaying_counter.hpp"
#include "nifpp.h"

static adroll::cookie_bouncer<adroll::decaying_counter> cb;

using std::make_tuple;
using std::ref;

extern "C" {

  static ERL_NIF_TERM cb_incr_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    try {
      std::string key;
      nifpp::get_throws(env, argv[0], key);

      cb.incr(key);

      nifpp::str_atom ok("ok");
      return nifpp::make(env, ok);
    }
    catch(nifpp::badarg) {}
    return enif_make_badarg(env);
  }

  static ERL_NIF_TERM cb_val_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    try {
      std::string key;
      nifpp::get_throws(env, argv[0], key);

      double val = cb.val(key);

      return nifpp::make(env, val);
    }
    catch(nifpp::badarg) {}
    return enif_make_badarg(env);
  }

  static ErlNifFunc nif_funcs[] = { {"incr", 1, cb_incr_nif},
                                    {"val", 1, cb_val_nif} };

  ERL_NIF_INIT(cookie_bouncer, nif_funcs, NULL, NULL, NULL, NULL)

} // extern "C"
