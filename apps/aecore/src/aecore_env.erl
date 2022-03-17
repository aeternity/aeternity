-module(aecore_env).

-export([ check_env/0
        , is_dev_mode/0
        , open_files_limit/0
        ]).

-export([patron_keypair_for_testing/0]).

%% Checking user-provided configs. The logic is somewhat complicated
%% by the fact that 'setup' is not guaranteed to start before lager,
%% so we have to be prepared to apply changes to both the lager env
%% and the (possibly) running lager. (This problem is solvable, but not
%% trivially. Basically, the aeternity.rel file must be pre-sorted and passed
%% to relx.
%%
check_env() ->
    dev_mode_expand(),
    check_open_files_limit(),
    aeu_env:check_env(
      aecore,
      [{[<<"mining">>, <<"autostart">>], fun set_autostart/2},
       {[<<"mining">>, <<"strictly_follow_top">>], {set_env, strictly_follow_top}},
       {[<<"mining">>, <<"attempt_timeout">>], {set_env, mining_attempt_timeout}},
       {[<<"chain">>, <<"persist">>]   , {set_env, persist}},
       {[<<"chain">>, <<"db_path">>]   , fun set_mnesia_dir/1}]).

is_dev_mode() ->
    case aeu_env:user_config([<<"system">>, <<"dev_mode">>]) of
        {ok, Bool} ->
            Bool;
        undefined ->
            case aeu_env:user_config([<<"fork_management">>, <<"network_id">>]) of
                {ok, <<"ae_dev">>} ->
                    true;
                _ ->
                    false
            end
    end.

check_open_files_limit() ->
    check_open_files_limit(aec_db:backend_mode()).

check_open_files_limit(#{module := mnesia_rocksdb} = Mode) ->
    case open_files_limit() of
	undefined ->
	    ok;
	Limit0 ->
	    case Limit0 - open_files_reserve() of
		Limit when Limit > 0 ->
		    application:set_env(mnesia_rocksdb, open_files_limit, Limit),
		    NumTabs = number_of_tables(Mode),
		    application:set_env(mnesia_rocksdb, number_of_tables, NumTabs);
		_ ->
		    ok
	    end
    end;
check_open_files_limit(_) ->
    %% Currently, we happily assume that the fd limit won't matter.
    ok.

%% Mnesia_rocksdb also treats indexes as tables
%% Also, add 1 for the mnesia_rocksdb admin table.
number_of_tables(Mode) ->
    Tabs = aec_db:tables(Mode),
    NTabs = length(Tabs),
    NIxs = lists:foldl(fun count_ixs/2, 1, Tabs).

count_ixs({_, Opts}, Acc) ->
    length(proplists:get_value(index, Opts, [])) + Acc.

open_files_limit() ->
    case os:getenv("AE_OPEN_FILES_LIMIT") of
        false ->
            undefined;
        LimitStr ->
            try list_to_integer(LimitStr) of
                Limit when is_integer(Limit) ->
                    Limit
            catch
                error:_ ->
                    lager:debug("Could not interpret ~p as limit", [LimitStr]),
                    undefined
            end
    end.

open_files_reserve() ->
    Default = 512,
    case os:getenv("AE_OPEN_FILES_RESERVE") of
        false -> Default;
        R ->
            try list_to_integer(R)
            catch
                error:_ ->
                    Default
            end
    end.

dev_mode_expand() ->
    case is_dev_mode() of
        true ->
            aeu_plugins:suggest_config([<<"mining">>,<<"strictly_follow_top">>], true);
        false ->
            ok
    end.

set_autostart(V, MMode) ->
    application:set_env(aecore, autostart, V andalso (not MMode)).

set_mnesia_dir(Path) ->
    MnesiaDir = filename:join(binary_to_list(Path), "mnesia"),
    ok = filelib:ensure_dir(MnesiaDir),
    application:set_env(mnesia, dir, MnesiaDir).

%% This key pair corresponds to the pubkey
%% ak_2a1j2Mk9YSmC1gioUq4PWRm3bsv887MbuRVwyv4KaUGoR1eiKi
%% defined in data/aecore/.genesis/accounts_test.json
%%
%% The key pair used to be hard-coded in aecore_suite_utils, but in order to
%% make it usable also in dev_mode, it is moved here.
%%
patron_keypair_for_testing() ->
    #{ pubkey  => <<206,167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,
                    73,187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>,
       privkey => <<230,169,29,99,60,119,207,87,113,50,157,51,84,179,188,239,27,
                    197,224,50,196,61,112,182,211,90,249,35,206,30,183,77,206,
                    167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,73,
                    187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>
     }.
