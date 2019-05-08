%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% Chain API for FATE
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_chain_api).

-export([ new/1
        ]).

%% Getters
-export([ account_balance/2
        , beneficiary/1
        , blockhash/2
        , contract_fate_code/2
        , difficulty/1
        , final_trees/1
        , gas_limit/1
        , gas_price/1
        , generation/1
        , origin/1
        , timestamp_in_msecs/1
        , tx_env/1
        ]).

%% Modifiers
-export([ spend/4
        ]).

-export_type([ state/0
             ]).

-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include("../../aecontract/include/aecontract.hrl").
-include("../../aecore/include/blocks.hrl").

%%%-------------------------------------------------------------------
%%% NOTE: We accept that this module causes havoc in the dependency
%%%       graph for now. The state/chain handling will move down to
%%%       a lower level in the dependency graph later.
%%%-------------------------------------------------------------------

-record(state, { primop_state :: aeprimop_state:state()
               , gas_price    :: non_neg_integer()
               , origin       :: binary()
               }).

-type state() :: #state{}.
-type pubkey() :: <<_:256>>.
%%===================================================================
%%% API
%%% ===================================================================

%%%-------------------------------------------------------------------
%%% External API for infrastructure

-spec new(map()) -> state().
new(#{ gas_price := GasPrice
     , origin := Origin
     , trees  := Trees
     , tx_env := TxEnv
     }) ->
    #state{ primop_state = aeprimop_state:new(Trees, TxEnv)
          , gas_price    = GasPrice
          , origin       = Origin
          }.

-spec tx_env(state()) -> aetx_env:env().
tx_env(#state{primop_state = PState}) ->
    aeprimop_state:tx_env(PState).

-spec final_trees(state()) -> aec_trees:trees().
final_trees(#state{primop_state = PState}) ->
    aeprimop_state:final_trees(PState).

%%%-------------------------------------------------------------------
%%% Basic getters

-spec origin(state()) -> aeb_fate_data:fate_address().
origin(#state{origin = Origin}) ->
    aeb_fate_data:make_address(Origin).

-spec gas_price(state()) -> aeb_fate_data:fate_integer().
gas_price(#state{gas_price = GasPrice}) ->
    aeb_fate_data:make_integer(GasPrice).

-spec beneficiary(state()) -> aeb_fate_data:fate_address().
beneficiary(#state{} = S) ->
    aeb_fate_data:make_address(aetx_env:beneficiary(tx_env(S))).

-spec generation(state()) -> aeb_fate_data:fate_integer().
generation(#state{} = S) ->
    aeb_fate_data:make_integer(aetx_env:height(tx_env(S))).

-spec difficulty(state()) -> aeb_fate_data:fate_integer().
difficulty(#state{} = S) ->
    aeb_fate_data:make_integer(aetx_env:difficulty(tx_env(S))).

-spec gas_limit(state()) -> aeb_fate_data:fate_integer().
gas_limit(#state{}) ->
    %% Should be tied to height if this is changed.
    aeb_fate_data:make_integer(aec_governance:block_gas_limit()).

-spec timestamp_in_msecs(state()) -> aeb_fate_data:fate_integer().
timestamp_in_msecs(#state{} = S) ->
    aeb_fate_data:make_integer(aetx_env:time_in_msecs(tx_env(S))).

%%%-------------------------------------------------------------------
%%% Slightly more involved getters with caching

-spec blockhash(non_neg_integer(), #state{}) -> aeb_fate_data:fate_integer().
blockhash(Height, #state{} = S) ->
    TxEnv = tx_env(S),
    case aetx_env:key_hash(TxEnv) of
        <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>> ->
            %% For channels
            aeb_fate_data:make_integer(0);
        KeyHash ->
            {ok, Header} = aec_chain:get_key_header_by_height(Height),
            {ok, Hash} = aec_headers:hash_header(Header),
            %% Make sure that this is an ancestor
            case aec_chain:find_common_ancestor(Hash, KeyHash) of
                {ok, <<N:?BLOCK_HEADER_HASH_BYTES/unit:8 >> = Hash} ->
                    aeb_fate_data:make_integer(N);
                {ok, _Other} ->
                    <<N:?BLOCK_HEADER_HASH_BYTES/unit:8 >> =
                        traverse_to_key_hash(Height, KeyHash),
                    aeb_fate_data:make_integer(N)
            end
    end.

traverse_to_key_hash(H, KeyHash) ->
    {ok, Header} = aec_chain:get_header(KeyHash),
    case aec_headers:height(Header) of
        Height when Height =:= H -> KeyHash;
        Height when Height =:= H + 1 -> aec_headers:prev_key_hash(Header);
        _Height -> traverse_to_key_hash(H, aec_headers:prev_key_hash(Header))
    end.

-spec contract_fate_code(pubkey(), state()) -> 'error' |
                                               {'ok', term(), state()}.
contract_fate_code(Pubkey, #state{primop_state = PState} = S) ->
    case aeprimop_state:find_contract_without_store(Pubkey, PState) of
        none -> error;
        {value, Contract} ->
            case aect_contracts:vm_version(Contract) of
                VMV when ?IS_FATE_SOPHIA(VMV) ->
                    SerCode = aect_contracts:code(Contract),
                    #{ byte_code := ByteCode} = aect_sophia:deserialize(SerCode),
                    try aeb_fate_asm:bytecode_to_fate_code(ByteCode, []) of
                        FateCode -> {ok, FateCode, S#state{primop_state = PState}}
                    catch _:_ -> error
                    end;
                _ ->
                    error
            end
    end.

-spec account_balance(pubkey(), state()) -> 'error' |
                                            {'ok', aeb_fate_data:fate_integer(), state()}.
account_balance(Pubkey, #state{primop_state = PState} = S) ->
    case aeprimop_state:find_account(Pubkey, PState) of
        {Account, PState1} ->
            Balance = aeb_fate_data:make_integer(aec_accounts:balance(Account)),
            {ok, Balance, S#state{primop_state = PState1}};
        none ->
            error
    end.

%%%-------------------------------------------------------------------
%%% Operations modifying state

spend(FromPubkey, ToPubkey, Amount, State) ->
    eval_primops([ aeprimop:spend_op(FromPubkey, ToPubkey, Amount)
                 ], State).

eval_primops(Ops, #state{primop_state = PState} = S) ->
    case aeprimop:eval_on_primop_state(Ops, PState) of
        {ok, PState1} ->
            {ok, S#state{primop_state = PState1}};
        {ok, Return, PState1} ->
            {ok, Return, S#state{primop_state = PState1}};
        {error, Atom} = Err when is_atom(Atom) ->
            Err
    end.

