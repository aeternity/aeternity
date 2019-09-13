%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------
%%% @doc
%%% ADT containing a check/process environment for transactions
%%% @end
%%%-------------------------------------------------------------------

-module(aetx_env).

%% Constructors
-export([ tx_env_from_key_header/4
        , tx_env_and_trees_from_hash/2
        , tx_env_and_trees_from_top/1
        ]).

-ifdef(TEST).
-export([ contract_env/6
        , tx_env/1
        ]).

-include("../../aecore/include/blocks.hrl").

-endif.

%% Getters
-export([ beneficiary/1
        , consensus_version/1
        , context/1
        , difficulty/1
        , dry_run/1
        , ga_auth_ids/1
        , ga_nonce/2
        , ga_tx_hash/1
        , height/1
        , key_hash/1
        , signed_tx/1
        , time_in_msecs/1
        , events/1
        ]).

%% Setters
-export([ add_ga_auth_id/2
        , add_ga_nonce/3
        , del_ga_auth_id/2
        , del_ga_nonce/2
        , set_beneficiary/2
        , set_consensus_version/2
        , set_context/2
        , set_dry_run/2
        , set_ga_tx_hash/2
        , set_height/2
        , set_signed_tx/2
        , tx_event/2
        , set_events/2
        , update_env/2
        ]).


%%%===================================================================
%%% Types
%%%===================================================================

%% Where does this transaction come from? Is it a top level transaction
%% or was it created by a smart contract. In the latter case the fee
%% logic is different. It can also be the inner transaction of a
%% ga_meta_tx, and a third set of rules apply.
-type context() :: 'aetx_transaction' | 'aetx_contract' | 'aetx_ga'.
-type wrapped_tx() :: {'value', aetx_sign:signed_tx()} | 'none'.
-type events() :: map().

-record(env, { consensus_version :: non_neg_integer()
             , beneficiary       :: aec_keys:pubkey()
             , context           :: context()
             , ga_auth_ids = []  :: [aec_keys:pubkey()]
             , ga_nonces = []    :: [{aec_keys:pubkey(), binary()}]
             , ga_tx_hash        :: undefined | binary()
             , difficulty        :: aeminer_pow:difficulty()
             , dry_run = false   :: boolean()
             , height            :: aec_blocks:height()
             , key_hash          :: aec_blocks:block_header_hash()
             , signed_tx         :: wrapped_tx()
             , time              :: non_neg_integer()
             , events = #{}      :: map()
             }).

-opaque env() :: #env{}.

-export_type([ env/0
             , context/0
             , wrapped_tx/0
             , events/0
             ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec tx_env_from_key_header(aec_headers:key_header(),
                             aec_blocks:block_header_hash(),
                             non_neg_integer(),
                             aec_blocks:block_header_hash()) ->
                             env().
tx_env_from_key_header(KeyHeader, KeyHash, Time,_PrevHash) ->
    #env{ beneficiary = aec_headers:beneficiary(KeyHeader)
        , consensus_version = aec_headers:version(KeyHeader)
        , context = aetx_transaction
        , difficulty = aec_headers:difficulty(KeyHeader)
        , height  = aec_headers:height(KeyHeader)
        , key_hash = KeyHash
        , signed_tx = none
        , time = Time
        }.

tx_env_and_trees_from_top(Type) when Type == aetx_contract;
                                     Type == aetx_transaction ->
    tx_env_and_trees_from_hash(Type, aec_chain:top_block_hash()).

tx_env_and_trees_from_hash(Type, Hash) when Type == aetx_contract;
                                            Type == aetx_transaction ->
    {ok, Header} = aec_chain:get_header(Hash),
    {ok, Trees}  = aec_chain:get_block_state(Hash),
    {KeyHeader, KeyHash, Time} =
        case aec_headers:type(Header) of
            micro ->
                KHash = aec_headers:prev_key_hash(Header),
                {ok, KH} = aec_chain:get_header(KHash),
                {KH, KHash, aec_headers:time_in_msecs(Header)};
            key ->
                {Header, Hash,
                 aec_headers:time_in_msecs(Header) +
                    aec_block_micro_candidate:min_t_after_keyblock()}
        end,
    Env = tx_env_from_key_header(KeyHeader, KeyHash, Time, Hash),
    {set_context(Env, Type), Trees}.


%%%===================================================================
%%% Test API
%%%===================================================================

-ifdef(TEST).

contract_env(Height, ConsensusVersion, Time, Beneficiary, Difficulty,
             KeyHash) ->
    #env{ beneficiary = Beneficiary
        , consensus_version = ConsensusVersion
        , context = aetx_contract
        , difficulty = Difficulty
        , height  = Height
        , key_hash = KeyHash
        , signed_tx = none
        , time = Time
     }.

tx_env(Height) ->
    Vsn = aec_hard_forks:protocol_effective_at_height(Height),
    #env{ consensus_version = Vsn
        , context = aetx_transaction
        , height  = Height
        , signed_tx = none
        , beneficiary = aec_block_genesis:beneficiary()
        , difficulty = aec_block_genesis:genesis_difficulty()
        , key_hash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>
        , time = aeu_time:now_in_msecs()
        }.

-endif.

%%%===================================================================
%%% Getters and setters

-spec beneficiary(env()) -> aec_keys:pubkey().
beneficiary(#env{beneficiary = X}) -> X.

-spec set_beneficiary(env(), aec_keys:pubkey()) -> env().
set_beneficiary(Env, X) -> Env#env{beneficiary = X}.

%%------

-spec consensus_version(env()) -> non_neg_integer().
consensus_version(#env{consensus_version = X}) -> X.

-spec set_consensus_version(env(), aec_hard_forks:protocol_vsn()) -> env().
set_consensus_version(Env, X) -> Env#env{consensus_version = X}.

%%------

-spec context(env()) -> context().
context(#env{context = X}) -> X.

-spec set_context(env(), context()) -> env().
set_context(Env, X) -> Env#env{context = X}.

%%------

-spec dry_run(env()) -> boolean().
dry_run(#env{dry_run = X}) -> X.

-spec set_dry_run(env(), boolean()) -> env().
set_dry_run(Env, X) -> Env#env{dry_run = X}.

%%------

-spec difficulty(env()) -> aeminer_pow:difficulty().
difficulty(#env{difficulty = X}) -> X.

%%------

-spec height(env()) -> aec_blocks:height().
height(#env{height = X}) -> X.

-spec set_height(env(), aec_blocks:height()) -> env().
set_height(Env, X) -> Env#env{height = X}.

%%------

-spec ga_auth_ids(env()) -> [aec_keys:pubkey()].
ga_auth_ids(#env{ga_auth_ids = X}) -> X.

-spec add_ga_auth_id(env(), aec_keys:pubkey()) -> env().
add_ga_auth_id(Env = #env{ga_auth_ids = Ids}, X) ->
    Env#env{ga_auth_ids = [X | Ids]}.

-spec del_ga_auth_id(env(), aec_keys:pubkey()) -> env().
del_ga_auth_id(Env = #env{ga_auth_ids = Ids}, X) ->
    Env#env{ga_auth_ids = Ids -- [X]}.

%%------

-spec ga_nonce(env(), aec_keys:pubkey()) -> none | {value, binary()}.
ga_nonce(#env{ga_nonces = X}, Pubkey) ->
    case lists:keyfind(Pubkey, 1, X) of
        false      -> none;
        {_, Nonce} -> {value, Nonce}
    end.

-spec add_ga_nonce(env(), aec_keys:pubkey(), binary()) -> env().
add_ga_nonce(Env = #env{ga_nonces = Xs}, Pk, Nonce) ->
    Env#env{ga_nonces = lists:keystore(Pk, 1, Xs, {Pk, Nonce})}.

-spec del_ga_nonce(env(), aec_keys:pubkey()) -> env().
del_ga_nonce(Env = #env{ga_nonces = Xs}, Pk) ->
    Env#env{ga_nonces = lists:keydelete(Pk, 1, Xs)}.

%%------

-spec ga_tx_hash(env()) -> undefined | binary().
ga_tx_hash(#env{ga_tx_hash = X}) -> X.

-spec set_ga_tx_hash(env(), undefined | binary()) -> env().
set_ga_tx_hash(Env, X) -> Env#env{ga_tx_hash = X}.

%%------

-spec tx_event(any(), env()) -> env().
tx_event(Name, #env{events = Events} = Env) ->
    case signed_tx(Env) of
        none -> Env;
        {value, SignedTx} ->
            TxHash = aetx_sign:hash(SignedTx),
            {Type, _} = aetx:specialize_type(aetx_sign:innermost_tx(SignedTx)),
            Env#env{events = Events#{Name => #{ type => Type
                                              , tx_hash => TxHash }}}
    end.

%%------

-spec set_events(env(), map()) -> env().
set_events(Env, Events) ->
    Env#env{events = Events}.

%%------

-spec update_env(env(), env()) -> env().
update_env(#env{events = Events}, ToEnv) ->
    ToEnv#env{events = Events}.

%%------

-spec key_hash(env()) -> aec_blocks:block_header_hash().
key_hash(#env{key_hash = X}) -> X.

%%------

-spec signed_tx(env()) -> wrapped_tx().
signed_tx(#env{signed_tx = X}) -> X.

-spec set_signed_tx(env(), wrapped_tx()) -> env().
set_signed_tx(Env, {value, _} = X) -> Env#env{signed_tx = X};
set_signed_tx(Env, none) ->  Env#env{signed_tx = none}.

%%------

-spec time_in_msecs(env()) -> non_neg_integer().
time_in_msecs(#env{time = X}) -> X.

%%------

-spec events(env()) -> map().
events(#env{events = X}) -> X.
