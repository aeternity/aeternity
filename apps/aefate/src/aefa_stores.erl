%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%    ADT for contract stores in FATE.
%%%
%%%    The code assumes that if a get/put is issued, the contract is
%%%    already in the store. Care must be taken to explicitly check if
%%%    a contract is present, and put it (using put_contract/2) if it is not.
%%%
%%%    Entries are cached in a read/write manner to avoid multiple
%%%    reading/converting and to avoid pushing terms that have not been
%%%    altered. Both things are expensive as it involves going out to the
%%%    underlying merkle trees.
%%%
%%%    Use finalize/2 to push the stores back to the chain when the
%%%    fate execution is done.
%%%
%%%  @end
%%%    -------------------------------------------------------------------

-module(aefa_stores).

-export([ finalize/2
        , find_value/3
        , has_contract/2
        , new/0
        , put_contract_store/3
        , put_value/4
        ]).

-record(store, { cache = #{} :: contract_cache()
               }).

-record(cache_entry, { store :: aect_contracts_store:store()
                     , dirty :: boolean()
                     , terms :: fate_terms()
                     }).

-type fate_val() :: aeb_fate_data:fate_type().
-type pubkey() :: <<_:256>>.
-type dirty() :: boolean().
-type fate_terms() :: #{ integer() => {fate_val(), dirty()} }.
-type contract_cache() :: #{pubkey() => #cache_entry{}}.

-opaque store() :: #store{}.

-export_type([ store/0
             ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> store().
new() ->
  #store{}.

-spec put_contract_store(pubkey(), aect_contracts_store:store(), store()) -> store().
put_contract_store(Pubkey, Store, #store{cache = Cache} = S) ->
    S#store{cache = Cache#{Pubkey => new_contract_cache_entry(Store)}}.

-spec has_contract(pubkey(), store()) -> boolean().
has_contract(Pubkey, #store{cache = Cache}) ->
    maps:is_key(Pubkey, Cache).

-spec find_value(pubkey(), non_neg_integer(), store()) ->
                    {'ok', fate_val(), store()}
                  | {'ok', fate_val()}
                  | 'error'.
find_value(Pubkey, StorePos, #store{cache = Cache} = S) ->
    case find_term(StorePos, maps:get(Pubkey, Cache)) of
        {ok, Term} ->
            {ok, Term};
        {ok, Term, Entry} ->
            {ok, Term, S#store{cache = #{Pubkey => Entry}}};
        error ->
            error
    end.

-spec put_value(pubkey(), non_neg_integer(), fate_val(), store()) -> store().
put_value(Pubkey, StorePos, FateVal, #store{cache = Cache} = S) ->
    Entry = maps:get(Pubkey, Cache),
    Terms = maps:put(StorePos, {FateVal, true}, Entry#cache_entry.terms),
    Entry1 = Entry#cache_entry{terms = Terms, dirty = true},
    S#store{cache = Cache#{Pubkey => Entry1}}.

%%%===================================================================
%%% Write through cache to stores

-spec finalize(aefa_chain_api:state(), store()) -> aefa_chain_api:state().
finalize(API, #store{cache = Cache}) ->
    Stores = maps:fold(fun finalize_entry/3, [], Cache),
    finalize_stores(Stores, API).

finalize_stores([{Pubkey, Store}|Left], API) ->
    API1 = aefa_chain_api:set_contract_store(Pubkey, Store, API),
    finalize_stores(Left, API1);
finalize_stores([], API) ->
    API.

finalize_entry(_Pubkey, #cache_entry{dirty = false}, Acc) ->
    Acc;
finalize_entry(Pubkey, #cache_entry{terms = Terms, store = Store}, Acc) ->
    [{Pubkey, maps:fold(fun push_term/3, Store, Terms)} | Acc].

push_term(StorePos, {FateVal, Dirty}, Store) when Dirty ->
    Val = aeb_fate_encoding:serialize(FateVal),
    aect_contracts_store:put(store_key(StorePos), Val, Store);
push_term(_StorePos, {_FateVal, Dirty}, Store) when not Dirty ->
    Store.

%%%===================================================================
%%% Entry for one contract

new_contract_cache_entry(Store) ->
    #cache_entry{ store = Store
                , terms = #{}
                , dirty = false
                }.

find_term(StorePos, #cache_entry{terms = Terms} = E) ->
    case maps:find(StorePos, Terms) of
        {ok, {FateVal,_Dirty}} ->
            {ok, FateVal};
        error ->
            case aect_contracts_store:get(store_key(StorePos), E#cache_entry.store) of
                <<>> ->
                    error;
                Value ->
                    FateVal = aeb_fate_encoding:deserialize(Value),
                    {ok, FateVal, E#cache_entry{terms = Terms#{StorePos => {FateVal, false}}}}
            end
    end.

-define(MAX_STORE_POS,
        16#ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff).

store_key(Int) when Int =< ?MAX_STORE_POS, Int > 0 ->
    binary:encode_unsigned(Int).
