-module(schedinfo2).

%%% @doc Scheduler info sneeky peeky NIF
%%%
%%% Try and work out what the scheduler is up to without hacking up beam.smp

-export([snoop_runq/1, snoop_runqs/0]). % Know what you're doing!
-on_load(on_load/0).
-export([reload/0]).
-export([check_balance_reds/0]).
-export([set_check_balance_reds/2]).
-export([schedulers_state/0]).
-export([active_schedulers/0]).

on_load() ->
    PrivDir = code:priv_dir(runtime_tools),
    LibName = "schedinfo2",
    Lib = filename:join([PrivDir, "lib", LibName]),
    case erlang:load_nif(Lib, 0) of
        ok -> ok;
        reload -> reload;
        {error, {load_failed, _}}=Error1 ->
            ArchLibDir = 
                filename:join([PrivDir, "lib", 
                               erlang:system_info(system_architecture)]),
            Candidate =
                filelib:wildcard(filename:join([ArchLibDir,LibName ++ "*" ])),
            case Candidate of
                [] -> Error1;
                _ ->
                    ArchLib = filename:join([ArchLibDir, LibName]),
                    erlang:load_nif(ArchLib, 0)
            end;
        Error1 -> Error1
    end.

reload() ->
    on_load().

snoop_runqs() ->
    [snoop_runq(SchedId) || SchedId <- lists:seq(1, erlang:system_info(schedulers))].

check_balance_reds() ->
    [proplists:get_value(check_balance_reds, RQ) || RQ <- snoop_runqs()].

%%%
%%% NIF placeholders
%%%

snoop_runq(_SchedId) ->
    erlang:nif_error(nif_not_loaded).

set_check_balance_reds(_SchedId, _Reds) ->
    erlang:nif_error(nif_not_loaded).

schedulers_state() ->
    erlang:nif_error(nif_not_loaded).

active_schedulers() ->
    erlang:nif_error(nif_not_loaded).
