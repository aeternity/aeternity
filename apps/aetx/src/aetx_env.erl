%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------
%%% @doc
%%% ADT containing a check/process environment for transactions
%%% @end
%%%-------------------------------------------------------------------

-module(aetx_env).

%% Constructors
-export([ tx_env/2
        , tx_env/3
        , contract_env/2
        ]).

%% Getters
-export([ consensus_version/1
        , context/1
        , height/1
        , signed_tx/1
        ]).

%% Setters
-export([ set_consensus_version/2
        , set_context/2
        , set_height/2
        , set_signed_tx/2
        ]).


%%%===================================================================
%%% Types
%%%===================================================================

%% Where does this transaction come from? Is it a top level transaction
%% or was it created by a smart contract. In the latter case the fee
%% logic is different.
-type context() :: 'aetx_transaction' | 'aetx_contract'.
-type wrapped_tx() :: {'value', aetx_sign:signed_tx()} | 'none'.

-record(env, { consensus_version :: non_neg_integer()
             , context           :: context()
             , height            :: aec_blocks:height()
             , signed_tx         :: wrapped_tx()
             }).

-opaque env() :: #env{}.

-export_type([ env/0
             , context/0
             , wrapped_tx/0
             ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec contract_env(aec_blocks:height(), non_neg_integer()) -> env().
contract_env(Height, ConsensusVersion) ->
    #env{ consensus_version = ConsensusVersion
        , context = aetx_contract
        , height  = Height
        , signed_tx = none
     }.

-spec tx_env(aec_blocks:height(), non_neg_integer()) -> env().
tx_env(Height, ConsensusVersion) ->
    #env{ consensus_version = ConsensusVersion
        , context = aetx_transaction
        , height  = Height
        , signed_tx = none
        }.

-spec tx_env(aec_blocks:height(), non_neg_integer(),
             {'value', aetx_sign:signed_tx()}) -> env().
tx_env(Height, ConsensusVersion, {value, _} = STx) ->
    #env{ consensus_version = ConsensusVersion
        , context = aetx_transaction
        , height  = Height
        , signed_tx = STx
        }.

%%%===================================================================
%%% Getters and setters

-spec consensus_version(env()) -> non_neg_integer().
consensus_version(#env{consensus_version = X}) -> X.

-spec set_consensus_version(env(), non_neg_integer()) -> env().
set_consensus_version(Env, X) -> Env#env{consensus_version = X}.

%%------

-spec context(env()) -> context().
context(#env{context = X}) -> X.

-spec set_context(env(), context()) -> env().
set_context(Env, X) -> Env#env{context = X}.

%%------

-spec height(env()) -> aec_blocks:height().
height(#env{height = X}) -> X.

-spec set_height(env(), aec_blocks:height()) -> env().
set_height(Env, X) -> Env#env{height = X}.

%%------

-spec signed_tx(env()) -> wrapped_tx().
signed_tx(#env{signed_tx = X}) -> X.

-spec set_signed_tx(env(), wrapped_tx()) -> env().
set_signed_tx(Env, {value, _} = X) -> Env#env{signed_tx = X};
set_signed_tx(Env, none) ->  Env#env{signed_tx = none}.
