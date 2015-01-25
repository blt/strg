-module(cookie_bouncer_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [
     sanity_test,
     create_and_incr,
     delete_table,
     sandblast,
     fsm_test
    ].

init_per_suite(Config) ->
    application:start(cookie_bouncer),
    Config.

end_per_suite(_Config) ->
    application:stop(cookie_bouncer),
    ok.

init_per_testcase(_Suite, Config) ->
    Config.

end_per_testcase(_Suite, _Config) ->
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

sanity_test(_Config) ->
    ?assertMatch(ok,                     cookie_bouncer:new(tbl, [])),
    ?assertMatch({error, no_such_table}, cookie_bouncer:val(no_such, "no such")),
    ?assertMatch({error, no_such_table}, cookie_bouncer:incr(no_such, "no such")).

create_and_incr(_Config) ->
    Tbl = create_and_incr_table,
    Key = "create_and_incr_key",

    ?assertMatch(ok, cookie_bouncer:new(Tbl, [])),
    ?assertMatch(0.0, cookie_bouncer:val(Tbl, Key)),
    _ = [cookie_bouncer:incr(Tbl, Key) || _ <- lists:seq(1,100)],
    ?assert(cookie_bouncer:val(Tbl, Key) > 95).

delete_table(_Config) ->
    Tbl = delete_table_table,
    Key = "key",

    ?assertMatch({error, no_such_table}, cookie_bouncer:val(Tbl, Key)),
    ?assertMatch(ok, cookie_bouncer:new(Tbl, [])),
    ?assertMatch(0.0, cookie_bouncer:val(Tbl, Key)),
    ?assertMatch(ok, cookie_bouncer:delete(Tbl)),
    ?assertMatch({error, no_such_table}, cookie_bouncer:val(Tbl, Key)).

sandblast(_Config) ->
    Tbl = sandblast_table,
    Key = "key",

    ?assertMatch(ok, cookie_bouncer:new(Tbl, [])),

    Parent = self(),

    spawn_link(fun() ->
                       _ = [cookie_bouncer:incr(Tbl, Key) || _ <- lists:seq(1,10000)],
                        Parent ! done
               end),
    spawn_link(fun() ->
                       _ = [cookie_bouncer:incr(Tbl, Key) || _ <- lists:seq(1,10000)],
                        Parent ! done
               end),
    spawn_link(fun() ->
                       _ = [cookie_bouncer:incr(Tbl, Key) || _ <- lists:seq(1,10000)],
                        Parent ! done
               end),
    spawn_link(fun() ->
                       _ = [cookie_bouncer:incr(Tbl, Key) || _ <- lists:seq(1,10000)],
                        Parent ! done
               end),

    ok = receive done -> ok end,
    ok = receive done -> ok end,
    ok = receive done -> ok end,
    ok = receive done -> ok end,

    ?assert(cookie_bouncer:val(Tbl, Key) > 39000),

    ok.

%%%===================================================================
%%% FSM Test
%%%===================================================================

-define(TOTWRKS, 10000).
-define(TOTKEYS, 10).
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
    [ok, ok, ok, ok, ok, ok, ok, ok] = [cookie_bouncer:new(Tbl, []) || Tbl <- Tbls],

    spawn_workers(Workers, nothing),
    wait_for_workers(Workers),

    ok.

spawn_workers(0, _) -> ok;
spawn_workers(N, State) ->
    Parent = self(),
    spawn_link(fun() ->
                       ok = init(State),
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
    ok = cookie_bouncer:incr(Tbl, Key),

    NextStates = [{500, fun incr_key/1}, {1000, fun read_val/1}],
    (prob_choice(NextStates))(State).

read_val(State) ->
    Key = rnd_key(),
    Tbl = rnd_tbl(),
    _ = cookie_bouncer:val(Tbl, Key),

    NextStates = [{10, fun init/1}, {505, fun incr_key/1}, {1000, fun read_val/1}],
    (prob_choice(NextStates))(State).

terminate(_State) ->
    ok.
