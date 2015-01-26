#include <iostream>

#include "cookie_bouncer.hpp"
#include "decaying_counter.hpp"
#include "nifpp.h"

#include <unordered_map>

#define UNUSED(expr) (void)(expr)

static std::unordered_map<nifpp::str_atom,
                          adroll::cookie_bouncer<adroll::decaying_counter>> meta_map;

static adroll::cookie_bouncer<adroll::decaying_counter> cb;

using std::make_tuple;
using std::ref;

extern "C" {

  static ERL_NIF_TERM cb_new_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    try {
      nifpp::str_atom atom_name;
      nifpp::get_throws(env, argv[0], atom_name);

      auto search = meta_map.find(atom_name);
      if (search == meta_map.end()) {
        meta_map.emplace(std::piecewise_construct,
                         std::forward_as_tuple(atom_name),
                         std::forward_as_tuple());
      }

      nifpp::str_atom ok("ok");
      return nifpp::make(env, ok);
    }
    catch(nifpp::badarg) {}
    return enif_make_badarg(env);
  }

  static ERL_NIF_TERM cb_delete_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    try {
      nifpp::str_atom atom_name;
      nifpp::get_throws(env, argv[0], atom_name);

      meta_map.erase(atom_name);
      nifpp::str_atom ok("ok");
      return nifpp::make(env, ok);
    }
    catch(nifpp::badarg) {}
    return enif_make_badarg(env);
  }

  static ERL_NIF_TERM cb_incr_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    try {
      nifpp::str_atom atom_name;
      nifpp::get_throws(env, argv[0], atom_name);

      std::string key;
      nifpp::get_throws(env, argv[1], key);

      auto search = meta_map.find(atom_name);
      if (search != meta_map.end()) {
        (search->second).incr(key);
        nifpp::str_atom ok("ok");
        return nifpp::make(env, ok);
      } else {
        nifpp::str_atom error("error");
        nifpp::str_atom no_such_tbl("no_such_table");
        auto tup = std::make_tuple(std::ref(error), std::ref(no_such_tbl));
        return nifpp::make(env, tup);
      }
    }
    catch(nifpp::badarg) {}
    return enif_make_badarg(env);
  }

  static ERL_NIF_TERM cb_val_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    UNUSED(argc);
    try {
      nifpp::str_atom atom_name;
      nifpp::get_throws(env, argv[0], atom_name);

      std::string key;
      nifpp::get_throws(env, argv[1], key);

      auto search = meta_map.find(atom_name);
      if (search != meta_map.end()) {
        double val = (search->second).val(key);
        return nifpp::make(env, val);
      } else {
        nifpp::str_atom error("error");
        nifpp::str_atom no_such_tbl("no_such_table");
        auto tup = std::make_tuple(std::ref(error), std::ref(no_such_tbl));
        return nifpp::make(env, tup);
      }
    }
    catch(nifpp::badarg) {}
    return enif_make_badarg(env);
  }

  static ErlNifFunc nif_funcs[] = { {"incr", 2, cb_incr_nif},
                                    {"new", 2, cb_new_nif},
                                    {"delete", 1, cb_delete_nif},
                                    {"val", 2, cb_val_nif} };

  ERL_NIF_INIT(cookie_bouncer, nif_funcs, NULL, NULL, NULL, NULL)

} // extern "C"