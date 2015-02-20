-module(strg).
-export([incr/2, info/1, info/2, val/2, delete/1, new/2]).
-on_load(init/0).

%% NOTE : this is a bad idea
priv_dir(Mod) ->
    Ebin = filename:dirname(code:which(Mod)),
    filename:join(filename:dirname(Ebin), "priv").

init() ->
    PrivDir = priv_dir(strg),
    ok = erlang:load_nif(filename:join(PrivDir, "strg"), 0).

new(Name, []) ->
    new_private(Name, -1.0, 10000);
new(Name, [{decay, false} | Rest]) ->
    LRUMaxSize = option(Rest, lru_max_size, 10000),
    new_private(Name, -1.0, LRUMaxSize);
new(Name, [{decay, true} | Rest]) ->
    LRUMaxSize = option(Rest, lru_max_size, 10000),
    new_private(Name, 60.0, LRUMaxSize);
new(Name, [{half_life, HL} | Rest]) when HL >= 0.0 ->
    LRUMaxSize = option(Rest, lru_max_size, 10000),
    new_private(Name, HL, LRUMaxSize).

info(Name) ->
    {size, Size} = info_private(Name),
    [{size, Size}].

info(Name, Key) ->
    InfoPL = info(Name),
    {Key, Value} = lists:keyfind(Key, 1, InfoPL),
    Value.

delete(_Name) ->
    exit(nif_library_not_loaded).

incr(_Tbl, _Key) ->
    exit(nif_library_not_loaded).

val(_Tbl, _Key) ->
    exit(nif_library_not_loaded).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% NOTE: Why does this function exist? I couldn't figure out how to part an
%% option list in NIF land.
new_private(_Name, _DecayBool, _LRUMaxSize) ->
    exit(nif_library_not_loaded).

info_private(_Name) ->
    exit(nif_library_not_loaded).

option(L, Key, Default) ->
    case lists:keyfind(Key, 1, L) of
        false -> Default;
        {Key, Other} -> Other
    end.
