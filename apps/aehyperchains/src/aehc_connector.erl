-module(aehc_connector).

-export([broadcast/4, verify/2, subscribe/2]).

-type tx_hash() :: binary().
-type tx_payload() :: binary().

-type account() :: <<_:256>>.

-type connector() :: atom().

%% TODO
-callback broadcast(From::account(), TxPayload::tx_payload(), To::account()) -> 
    tx_hash().

-callback verify(TxHash::tx_hash()) -> 
    boolean().

-callback subscribe(Module::atom()) -> ok.

-spec broadcast(Con::connector(), From::account(), TxPayload::tx_payload(), To::account()) -> 
                       tx_hash().
broadcast(Con, From, TxPayload, To) ->
    %% Additional params (like value, fee) are parent related and can be passed via appropriate config;
    Res = Con:broadcast(From, TxPayload, To), true = is_binary(Res),
    Res.

-spec verify(Con::connector(), TxHash::tx_hash()) -> 
                    boolean().
verify(Con, TxHash) ->
    Res = Con:verify(TxHash), true = is_boolean(Res), 
    Res.

-spec subscribe(Con::connector(), Module::atom()) -> ok.
subscribe(Con, Module) ->
    ok = Con:subscribe(Module).
