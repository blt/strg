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

new(Name, []) ->
    new_private(Name, -1.0);
new(Name, [{decay, false}]) ->
    new_private(Name, -1.0);
new(Name, [{decay, true}]) ->
    new_private(Name, 60.0);
new(Name, [{half_life, HL}]) when HL >= 0.0 ->
    new_private(Name, HL).

%% NOTE: Why does this function exist? I couldn't figure out how to part an
%% option list in NIF land.
new_private(_Name, _Options) ->
    exit(nif_library_not_loaded).

delete(_Name) ->
    exit(nif_library_not_loaded).

incr(_Tbl, _Key) ->
    exit(nif_library_not_loaded).

val(_Tbl, _Key) ->
    exit(nif_library_not_loaded).
