-module(schedinfo).

%%% @doc Scheduler info sneeky peeky NIF
%%%
%%% Try and work out what the scheduler is up to without hacking up beam.smp

-export([snoop_runq/1]). % Know what you're doing!
-on_load(on_load/0).

on_load() ->
    PrivDir = code:priv_dir(runtime_tools),
    LibName = "schedinfo",
    Lib = filename:join([PrivDir, "lib", LibName]),
    case erlang:load_nif(Lib, 0) of
        ok -> ok;
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

%%%
%%% NIF placeholders
%%%

snoop_runq(_SchedId) ->
    erlang:nif_error(nif_not_loaded).
