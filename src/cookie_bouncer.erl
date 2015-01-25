-module(cookie_bouncer).
-export([incr/2, val/2, delete/1, new/2]).
-on_load(init/0).

%% NOTE : this is a bad idea
priv_dir(Mod) ->
    Ebin = filename:dirname(code:which(Mod)),
    filename:join(filename:dirname(Ebin), "priv").

init() ->
    PrivDir = priv_dir(cookie_bouncer),
    ok = erlang:load_nif(filename:join(PrivDir, "cookie_bouncer"), 0).

new(_Name, _Options) ->
    exit(nif_library_not_loaded).

delete(_Name) ->
    exit(nif_library_not_loaded).

incr(_Tbl, _Key) ->
    exit(nif_library_not_loaded).

val(_Tbl, _Key) ->
    exit(nif_library_not_loaded).
