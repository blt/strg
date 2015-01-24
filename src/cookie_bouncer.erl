-module(cookie_bouncer).
-export([incr/1, val/1]).
-on_load(init/0).

%% NOTE : this is a bad idea
priv_dir(Mod) ->
    Ebin = filename:dirname(code:which(Mod)),
    filename:join(filename:dirname(Ebin), "priv").

init() ->
    PrivDir = priv_dir(cookie_bouncer),
    ok = erlang:load_nif(filename:join(PrivDir, "cookie_bouncer"), 0).

incr(_T) ->
    exit(nif_library_not_loaded).

val(_T) ->
    exit(nif_library_not_loaded).
