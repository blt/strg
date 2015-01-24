-module(cookie_bouncer).
-export([incr/1, val/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./cookie_bouncer", 0).

incr(_T) ->
    exit(nif_library_not_loaded).

val(_T) ->
    exit(nif_library_not_loaded).
