%% @doc This modules provides the API to start the internal client processes
%% based on the roles which are intended to be used.
-module(aesc_client).

-export([ initiate/3
        , respond/2
        ]).

-include_lib("aeutils/include/aeu_stacktrace.hrl").
-include("aechannel.hrl").

%% ==================================================================
%% API

initiate(Host, Port, #{} = Opts0) ->
    lager:debug("initiate(~p, ~p, ~p)", [Host, Port, aesc_utils:censor_init_opts(Opts0)]),
    Opts = maps:merge(#{client => self(), role => initiator}, Opts0),
    try init_checks(Opts) of
        ok ->
            aesc_fsm_sup:start_child([#{ host => Host
                                       , port => Port
                                       , opts => Opts }]);
        {error, _Reason} = Err ->
            Err
    ?CATCH_LOG(_E)
        {error, _E}
    end.

respond(Port, #{} = Opts0) ->
    lager:debug("respond(~p, ~p)", [Port, aesc_utils:censor_init_opts(Opts0)]),
    Opts = maps:merge(#{ client => self()
                       , role   => responder }, Opts0),
    try init_checks(Opts) of
        ok ->
            aesc_fsm_sup:start_child([#{ port => Port
                                       , opts => Opts }]);
        {error, _Reason} = Err ->
            Err
    ?CATCH_LOG(_E)
        {error, _E}
    end.

%% ==================================================================
%% Internal functions

init_checks(#{existing_channel_id := ChId, offchain_tx := Tx, role := Role})
  when is_binary(ChId) andalso Tx =/= undefined ->
    Checks = [ fun() -> aesc_checks:known_role(Role) end
             ],
    aeu_validation:run(Checks);
init_checks(Opts) ->
    #{ initiator   := Initiator
     , responder   := Responder
     , role        := Role
     , lock_period := LockPeriod } = Opts,
    Checks = [ fun() -> aesc_checks:known_role(Role) end
             , fun() -> aesc_checks:amounts(Opts) end
             , fun() -> aesc_checks:accounts(Initiator, Responder, Role) end
             , fun() -> aesc_checks:lock_period(LockPeriod) end
             ],
    aeu_validation:run(Checks).
