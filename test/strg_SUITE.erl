-module(strg_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [
     sanity_test
     , lru_test
     , decaying_tables_decay
     , counter_tables_do_not_decay
     , create_and_incr
     , delete_table
     , sandblast
     , fsm_test
    ].

init_per_suite(Config) ->
    application:start(strg),
    Config.

end_per_suite(_Config) ->
    application:stop(strg),
    ok.

init_per_testcase(_Suite, Config) ->
    Config.

end_per_testcase(_Suite, _Config) ->
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

sanity_test(_Config) ->
    ?assertMatch(ok,                     strg:new(tbl, [])),
    ?assertMatch(ok,                     strg:new(tbl_no_decay_bool, [{decay, false}])),
    ?assertMatch(ok,                     strg:new(tbl_decay_bool,    [{decay, true}])),
    ?assertMatch(ok,                     strg:new(tbl_decay_explicit, [{half_life, 6.0}])),
    ?assertMatch({error, no_such_table}, strg:val(no_such, "no such")),
    ?assertMatch({error, no_such_table}, strg:incr(no_such, "no such")).

lru_test(_Config) ->
    Tbl = lru_test_tbl,
    ok = strg:new(Tbl, [{decay, false}, {lru_max_size, 4}]),

    %% load the cookie bouncer up to lru_max_size
    strg:incr(Tbl, "one"),
    strg:incr(Tbl, "two"),
    strg:incr(Tbl, "three"),
    strg:incr(Tbl, "four"),

    %% confirm the values are in place
    ?assertMatch(1.0, strg:val(Tbl, "one")),
    ?assertMatch(1.0, strg:val(Tbl, "two")),
    ?assertMatch(1.0, strg:val(Tbl, "three")),
    ?assertMatch(1.0, strg:val(Tbl, "four")),

    %% push it past the post
    strg:incr(Tbl, "five"),

    %% confirm we've lost a value
    ?assertMatch(0.0, strg:val(Tbl, "one")),
    ?assertMatch(1.0, strg:val(Tbl, "two")),
    ?assertMatch(1.0, strg:val(Tbl, "three")),
    ?assertMatch(1.0, strg:val(Tbl, "four")),
    ?assertMatch(1.0, strg:val(Tbl, "five")),

    ok.

decaying_tables_decay(_Config) ->
    Tbl = decaying_tables_decay_tbl,
    Key = "k",

    ?assertMatch(ok, strg:new(Tbl, [{half_life, 1.00}])),
    {ok, 1.0} = strg:incr(Tbl, Key),
    {ok, 2.0} = strg:incr(Tbl, Key),
    timer:sleep(1000),
    ?assert(strg:val(Tbl, Key) < 2.0).

counter_tables_do_not_decay(_Config) ->
    Tbl = counter_tables_do_not_decay_tbl,
    Key = "k",

    ?assertMatch(ok, strg:new(Tbl, [{decay, false}])),
    {ok, 1.0} = strg:incr(Tbl, Key),
    {ok, 2.0} = strg:incr(Tbl, Key),
    timer:sleep(500),
    ?assertMatch(2.0, strg:val(Tbl, Key)).


create_and_incr(_Config) ->
    Tbl = create_and_incr_table,
    Key = "create_and_incr_key",

    ?assertMatch(ok, strg:new(Tbl, [])),
    ?assertMatch(0.0, strg:val(Tbl, Key)),
    _ = [strg:incr(Tbl, Key) || _ <- lists:seq(1,100)],
    ?assert(strg:val(Tbl, Key) > 95).

delete_table(_Config) ->
    Tbl = delete_table_table,
    Key = "key",

    ?assertMatch({error, no_such_table}, strg:val(Tbl, Key)),
    ?assertMatch(ok, strg:new(Tbl, [])),
    ?assertMatch(0.0, strg:val(Tbl, Key)),
    ?assertMatch(ok, strg:delete(Tbl)),
    ?assertMatch({error, no_such_table}, strg:val(Tbl, Key)).

sandblast(_Config) ->
    Tbl = sandblast_table,
    Key = "key",

    ?assertMatch(ok, strg:new(Tbl, [])),

    Parent = self(),

    spawn_link(fun() ->
                       _ = [strg:incr(Tbl, Key) || _ <- lists:seq(1,10000)],
                        Parent ! done
               end),
    spawn_link(fun() ->
                       _ = [strg:incr(Tbl, Key) || _ <- lists:seq(1,10000)],
                        Parent ! done
               end),
    spawn_link(fun() ->
                       _ = [strg:incr(Tbl, Key) || _ <- lists:seq(1,10000)],
                        Parent ! done
               end),
    spawn_link(fun() ->
                       _ = [strg:incr(Tbl, Key) || _ <- lists:seq(1,10000)],
                        Parent ! done
               end),

    ok = receive done -> ok end,
    ok = receive done -> ok end,
    ok = receive done -> ok end,
    ok = receive done -> ok end,

    ?assert(strg:val(Tbl, Key) > 39000),

    ok.

%%%===================================================================
%%% FSM Test
%%%===================================================================

-define(TOTWRKS, 1000).
-define(TOTKEYS, 100).
-define(TOTTBLS, 8).

rnd_key() -> integer_to_list(random:uniform(?TOTKEYS)).
rnd_tbl() -> list_to_atom(lists:flatten(["fsm_test_table_",
                                         integer_to_list(random:uniform(?TOTTBLS)-1)])).

prob_choice(L) ->
    Prob = random:uniform(1000),
    [{_,H}|_] = lists:dropwhile(fun({P, _El}) -> P < Prob end, L),
    H.

fsm_test(_Config) ->
    Workers = ?TOTWRKS,

    Tbls = [fsm_test_table_0, fsm_test_table_1, fsm_test_table_2, fsm_test_table_3,
            fsm_test_table_4, fsm_test_table_5, fsm_test_table_6, fsm_test_table_7],
    ?assertMatch(?TOTTBLS, length(Tbls)),
    [ok, ok, ok, ok, ok, ok, ok, ok] = [strg:new(Tbl, []) || Tbl <- Tbls],

    spawn_workers(Workers, nothing),
    wait_for_workers(Workers),

    ok.

spawn_workers(0, _) -> ok;
spawn_workers(N, State) ->
    Parent = self(),
    spawn_link(fun() ->
                       init(State),
                       Parent ! N
               end),
    spawn_workers(N-1, State).

wait_for_workers(0) -> ok;
wait_for_workers(N) -> receive _ID -> wait_for_workers(N-1) end.

init(State) ->
    NextStates = [{10, fun terminate/1},
                  {505, fun incr_key/1},
                  {1000, fun read_val/1}],
    (prob_choice(NextStates))(State).

incr_key(State) ->
    Key = rnd_key(),
    Tbl = rnd_tbl(),
    {ok, _} = strg:incr(Tbl, Key),

    NextStates = [{500, fun incr_key/1}, {1000, fun read_val/1}],
    (prob_choice(NextStates))(State).

read_val(State) ->
    Key = rnd_key(),
    Tbl = rnd_tbl(),
    _ = strg:val(Tbl, Key),

    NextStates = [{10, fun init/1}, {505, fun incr_key/1}, {1000, fun read_val/1}],
    (prob_choice(NextStates))(State).

terminate(_State) ->
    ok.
