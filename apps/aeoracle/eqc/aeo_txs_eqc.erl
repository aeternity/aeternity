%%% File        : aeo_txs_eqc.erl
%%% Author      : Hans Svensson
%%% Description :
%%% Created     : 18 Dec 2017 by Hans Svensson
-module(aeo_txs_eqc).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-record(state,
       { oracles = []
       , queries = []
       , responses = []
       , accounts = []
       , patron
       , height = 0
       }).

-record(account, { pubkey, balance, height, privkey, nonce = 0 }).

-record(oracle,
       { fee, address, ttl, height }).

-record(query,
       { oracle, query, fee, answered = false, query_owner,
         query_ttl, response_ttl, height, id }).

initial_state() ->
    #state{}.

%% -- Generators -------------------------------------------------------------
gen_oracle() ->
    #oracle{ fee = choose(1, 10)
           , ttl = {delta, frequency([{1, choose(3, 15)}, {9, choose(15, 50)}])} }.

gen_query(O) ->
    #query{ fee = O#oracle.fee, query = "Bla, bla...",
            oracle = O#oracle.address, query_ttl = gen_ttl(), response_ttl = gen_ttl() }.

gen_response() ->
    ?LET(N, nat(), list_to_binary(integer_to_list(N))).

%% TODO: fixed block
gen_ttl() ->
    {delta, choose(3, 10)}.

gen_key_pair() ->
    return(crypto:generate_key(ecdh, crypto:ec_curve(secp256k1))).

gen_account(AtHeight) ->
    ?LET({PubKey, PrivKey}, gen_key_pair(),
         #account{ pubkey = PubKey, privkey = PrivKey,
                   balance = choose(5, 200), height = AtHeight }).

%% -- Operations -------------------------------------------------------------

%% --- init ---
init_pre(S) ->
    S#state.patron == undefined.

init_args(_S) ->
    ?LET({PubKey, PrivKey}, gen_key_pair(),
         [#account{ pubkey = PubKey, balance = 1000000, height = 0, privkey = PrivKey }]).

init(#account{ pubkey = PK, balance = B }) ->
    {Genesis, Trees} = aec_block_genesis:genesis_block_with_state(#{ preset_accounts => [{PK, B}] }),
    {ok, GenesisHash} = aec_blocks:hash_internal_representation(Genesis),
    Chain = aec_chain_state:new_from_persistance([Genesis], [{GenesisHash, Trees}]),
    state_start(Chain).

init_next(S, _V, [Patron]) ->
    S#state{ patron = Patron }.

%% --- add_account ---
add_account_args(S) ->
    [S#state.patron, gen_account(S#state.height + 1)].

add_account_pre(S, [_Patron, NewAccount]) ->
    S#state.height + 1 == NewAccount#account.height.

add_account_adapt(S, [Patron, NewAccount]) ->
    [Patron, NewAccount#account{ height = S#state.height + 1}].

add_account(Patron, NewAccount) ->
    apply_tx(mk_spend_tx(Patron, NewAccount)).

add_account_next(S, _V, [Patron = #account{ balance = PB, nonce = PN },
                         NewAccount = #account{ balance = NB }]) ->
  S#state{ patron   = Patron#account{ balance = PB - NB - 1, nonce = PN + 1 },
           accounts = S#state.accounts ++ [NewAccount],
           height   = S#state.height + 1 }.

add_account_post(_S, [_Patron, _NewAccount], Res) ->
  eq(Res, ok).

%% --- register_oracle ---
register_oracle_pre(S) ->
    non_oracle_accounts(S) =/= [].

register_oracle_args(S) ->
    [elements(non_oracle_accounts(S)), gen_oracle()].

register_oracle_pre(S, [Account, _Oracle]) ->
    lists:member(Account, non_oracle_accounts(S)).

register_oracle(Account, Oracle) ->
    apply_tx(mk_oracle_register_tx(Account, Oracle)).

register_oracle_next(S, _V, [A = #account{ nonce = N, balance = B, pubkey = PK },
                             O = #oracle { ttl = TTL }]) ->
    case balance(PK, S) >= 4 + ttl_fee(TTL) of
        true ->
            S1 = update_account(A#account{ nonce = N+1, balance = B - (4 + ttl_fee(TTL)) }, S),
            S2 = update_oracle(O#oracle{ address = PK, height = S#state.height + 1 }, S1),
            S2#state{ height = S#state.height + 1 };
        false ->
            S
    end.

register_oracle_post(S, [#account{ pubkey = A }, #oracle{ ttl = TTL }], Res) ->
    case Res of
        ok -> true;
        {bad_tx, _Tx, _} ->
            case balance(A, S) < 4 + ttl_fee(TTL) of
                true  -> true;
                false -> eq(Res, ok)
            end
    end.


register_oracle_features(S, [#account{ pubkey = A }, #oracle{ ttl = TTL }], _V) ->
    case balance(A, S) < 4 + ttl_fee(TTL) of
        true -> [{register_oracle, false}];
        false -> [{register_oracle, true}]
    end.

%% --- query_oracle ---
query_oracle_pre(S) ->
    S#state.oracles =/= [].

query_oracle_args(S) ->
    ?LET(O, elements(S#state.oracles), [elements(S#state.accounts), gen_query(O)]).

query_oracle_pre(S, [Account, Query]) ->
    lists:member(Account, S#state.accounts)
        andalso lists:keymember(Query#query.oracle, #oracle.address, S#state.oracles).

query_oracle_adapt(S, [Account, Query]) ->
    case lists:keymember(Query#query.oracle, #oracle.address, S#state.oracles) andalso
        lists:keymember(Account#account.pubkey, #account.pubkey, S#state.accounts) of
        true ->
            [lists:keyfind(Account#account.pubkey, #account.pubkey, S#state.accounts), Query];
        false ->
            false
    end.

query_oracle(Account, Query) ->
    QueryTx = mk_oracle_query_tx(Account, Query),
    {oracle_query_tx, QTx} = aetx:specialize_type(QueryTx),
    Id = aeo_query_tx:query_id(QTx),
    {apply_tx(QueryTx), Id}.

query_oracle_next(S, Id, [A = #account{ balance = B, nonce = N, pubkey = PK },
                          Q = #query{ fee = Fee, query_ttl = TTL }]) ->
    case query_tx_status(S, A, Q) of
        ok ->
            S1 = update_account(A#account{ nonce = N+1, balance = B - (2 + Fee + ttl_fee(TTL)) }, S),
            S2 = update_query(Q#query{ query_owner = PK, height = S#state.height + 1,
                                       id = {call, erlang, element, [2, Id]} }, S1),
            S2#state{ height = S#state.height + 1 };
        _ ->
            S
    end.

query_oracle_post(S, [A, Q], {Res, _}) ->
    QueryStatus = query_tx_status(S, A, Q),
    case Res of
        ok -> eq(ok, QueryStatus);
        {bad_tx, _Tx, _} when QueryStatus == ok -> eq(bad_tx, ok);
        _ -> true
    end.

query_oracle_features(S, [A, Q], _V) ->
    [{query_oracle, query_tx_status(S, A, Q)}].

query_tx_status(S, #account{ pubkey = APK }, #query{ fee = QF, query_ttl = TTL, response_ttl = RTTL, oracle = OPK }) ->
    Oracle = lists:keyfind(OPK, #oracle.address, S#state.oracles),
    check([check_balance(balance(APK, S), QF + 2 + ttl_fee(TTL)),
           check_query_ttl(Oracle, TTL, RTTL, S#state.height+1),
           check_expire(oracle_expired(Oracle, S#state.height+1))]).

check_query_ttl(O, QTTL, RTTL, Height) ->
    OracleExpire = aeo_utils:ttl_expiry(O#oracle.height, O#oracle.ttl),
    QueryMaxExpire = aeo_utils:ttl_expiry(Height + aeo_utils:ttl_delta(Height, QTTL), RTTL),
    case OracleExpire < QueryMaxExpire of
        false -> ok;
        true  -> {error, query_ttl_too_long}
    end.

%% --- oracle_response ---

oracle_response_pre(S) ->
    S#state.queries =/= [].

oracle_response_args(S) ->
    ?LET(Q, elements(S#state.queries),
         [lists:keyfind(Q#query.oracle, #account.pubkey, S#state.accounts),
          Q, gen_response()]).

oracle_response_pre(S, [A, Q, _R]) ->
    A#account.pubkey == Q#query.oracle
        andalso lists:member(A, S#state.accounts)
        andalso lists:member(Q, S#state.queries).

oracle_response_adapt(S, [A, Q, R]) ->
    case lists:keymember(A#account.pubkey, #account.pubkey, S#state.accounts) andalso
        lists:keymember(Q#query.id, #query.id, S#state.queries) of
        true ->
            [lists:keyfind(A#account.pubkey, #account.pubkey, S#state.accounts),
             lists:keyfind(Q#query.id, #query.id, S#state.queries), R];
        false ->
            false
    end.

oracle_response(Account, Query, Response) ->
    ResponseTx = mk_oracle_response_tx(Account, Query, Response),
    apply_tx(ResponseTx).

oracle_response_next(S, _V, [A = #account{ balance = B, nonce = N },
                             Q = #query{ fee = QF, response_ttl = TTL }, _]) ->
    case response_tx_status(S, A, Q) of
        ok ->
            S1 = update_account(A#account{ nonce = N+1, balance = B - (2 + ttl_fee(TTL)) + QF }, S),
            S2 = update_query(Q#query{ answered = true }, S1),
            S2#state{ height = S#state.height + 1 };
        _ ->
            S
    end.

oracle_response_post(S, [A, Q, _R], Res) ->
    ResponseStatus = response_tx_status(S, A, Q),
    case Res of
        ok -> eq(ok, ResponseStatus);
        {bad_tx, _Tx, _} when ResponseStatus == ok -> eq(bad_tx, ok);
        _ -> true
    end.

oracle_response_features(S, [A, Q, _Response], _V) ->
    [{oracle_response, response_tx_status(S, A, Q)}].

response_tx_status(S, #account{ pubkey = APK }, Q = #query{ fee = QF, response_ttl = TTL }) ->
    Oracle = lists:keyfind(APK, #oracle.address, S#state.oracles),
    check([check_balance(balance(APK, S),  (2 + ttl_fee(TTL)) - QF),
           check_expire(query_expired(Q, S#state.height+1)),
           check_expire(oracle_expired(Oracle, S#state.height+1)),
           check_answered(Q)]).

check_balance(B1, B2) when B1 >= B2 -> ok;
check_balance(_B1, _B2) -> {error, insufficient_funds}.

check_expire(true)  -> {error, expired};
check_expire(false) -> ok.

check_answered(#query{ answered = true }) -> {error, already_answered};
check_answered(_) -> ok.

check([])        -> ok;
check([ok | Xs]) -> check(Xs);
check([E | _])   -> E.


%% -- Common pre-/post-conditions --------------------------------------------
command_precondition_common(S, Cmd) ->
    S#state.patron =/= undefined orelse Cmd == init.

invariant(#state{ patron = undefined }) -> true;
invariant(S) ->
    Chain = state_get(),
    {_LastBlock, Trees} = top_block_with_state(Chain),
    eqc_statem:conj([tag(accounts, check_accounts(S#state.accounts, Trees)),
                     tag(oracles, check_oracles(S#state.oracles, Trees, S#state.height)),
                     tag(queries, check_queries(S#state.queries, Trees, S#state.height))]).

oracle_expired(#oracle{ height = H0, ttl = TTL }, H) ->
    expired(H0, TTL, H).

query_expired(#query{ height = H0, query_ttl = TTL }, H) ->
    expired(H0, TTL, H).

expired(H0, TTL, H) ->
    H > aeo_utils:ttl_expiry(H0, TTL).

check_accounts(As, Trees) ->
    ATree = aec_trees:accounts(Trees),
    case lists:usort([ check_account(A, ATree) || A <- As ]) -- [true] of
        []     -> true;
        Err    -> Err
    end.

check_oracles(ModelOs, Trees, Height) ->
    OTree = aec_trees:oracles(Trees),
    ExpectedOs = [ O#oracle.address || O <- ModelOs, not oracle_expired(O, Height) ],
    ActualOs   = [ aeo_oracles:owner(O) || O <- aeo_state_tree:oracle_list(OTree)],
    case {ExpectedOs -- ActualOs, ActualOs -- ExpectedOs} of
        {[], []}   -> true;
        {[], Os}   -> {extra_oracles_in_state_tree, Os};
        {Os, []}   -> {premature_pruning, Os};
        {Os1, Os2} -> {extra, Os1, missing, Os2}
    end.

check_queries(ModelQs, Trees, Height) ->
    OTree = aec_trees:oracles(Trees),
    ExpectedQs = [ Q#query.id || Q <- ModelQs, not query_expired(Q, Height) ],
    ActualQs   = [ aeo_query:id(Q) || Q <- aeo_state_tree:query_list(OTree)],
    case {ExpectedQs -- ActualQs, ActualQs -- ExpectedQs} of
        {[], []}   -> true;
        {[], Qs}   -> {extra_queries_in_state_tree, Qs};
        {Qs, []}   -> {premature_pruning, Qs};
        {Qs1, Qs2} -> {extra, Qs1, missing, Qs2}
    end.

check_account(#account{ pubkey = PK, balance = B, nonce = N }, ATree) ->
    case aec_accounts_trees:lookup(PK, ATree) of
        none -> {account_missing, PK};
        {value, Account} ->
            tag(PK, eqc_statem:conj([tag(balance, eq(aec_accounts:balance(Account), B)),
                                     tag(nonce, eq(aec_accounts:nonce(Account), N))]))
    end.


%% -- Property ---------------------------------------------------------------
weight(_S, oracle_response) -> 2;
weight(_S, query_oracle) -> 2;
weight(_S, _Cmd) -> 1.

prop_ok() ->
    ?SETUP(fun() -> setup(), fun() -> teardown() end end,
    ?FORALL(Cmds, commands(?MODULE),
    %% eqc_statem:show_states(
    begin
        HSR = {H, _S, Res} = run_commands(Cmds),
        check_command_names(Cmds,
            measure(length, commands_length(Cmds),
            aggregate(call_features(H),
            pretty_commands(?MODULE, Cmds, HSR, Res == ok))))
    end)).

setup() ->
    eqc_mocking:start_mocking(api_spec()).

teardown() ->
    eqc_mocking:stop_mocking().

tag(_, true) -> true;
tag(Tag, X)  -> {Tag, X}.

api_spec() ->
    #api_spec{
        modules = [aec_tx_sign(), aec_target()]
    }.

aec_target() ->
    #api_module{ name = aec_target, fallback = aeo_oracles_mock }.

aec_tx_sign() ->
    #api_module{ name = aec_tx_sign, fallback = aeo_oracles_mock }.

%% -- Transaction helpers ----------------------------------------------------
apply_tx(Tx) ->
    Chain = state_get(),
    {LastBlock, Trees0} = top_block_with_state(Chain),
    try
        NewBlock = aec_blocks:new(LastBlock, [Tx], Trees0),
        {ok, Chain1} = aec_chain_state:insert_block(NewBlock, Chain),
        state_put(Chain1),
        ok
    catch E:R ->
        {bad_tx, Tx, {E, R, erlang:get_stacktrace()}}
    end.

mk_spend_tx(Sender, Receiver) ->
    {ok, Tx} =
        aec_spend_tx:new(#{ sender    => Sender#account.pubkey,
                            recipient => Receiver#account.pubkey,
                            amount    => Receiver#account.balance,
                            fee       => 1,
                            nonce     => Sender#account.nonce + 1 }),
    Tx.

mk_oracle_register_tx(#account{ pubkey = PK, nonce = N },
                      #oracle{ fee = QF, ttl = TTL }) ->
    {ok, Tx} =
        aeo_register_tx:new(#{account       => PK,
                              nonce         => N + 1,
                              query_spec    => <<"string()">>,
                              response_spec => <<"boolean() | integer()">>,
                              query_fee     => QF,
                              ttl           => TTL,
                              fee           => 4 + ttl_fee(TTL)}),
    Tx.

mk_oracle_query_tx(#account{ pubkey = PK, nonce = N },
                   #query{ fee = QF, query_ttl = TTL, response_ttl = RTTL, oracle = O, query = Q }) ->
    {ok, Tx} =
        aeo_query_tx:new(#{sender        => PK,
                           nonce         => N + 1,
                           oracle        => O,
                           query         => list_to_binary(Q),
                           query_fee     => QF,
                           query_ttl     => TTL,
                           response_ttl  => RTTL,
                           fee           => 2 + ttl_fee(TTL)}),
    Tx.

mk_oracle_response_tx(#account{ pubkey = PK, nonce = N },
                      #query{ response_ttl = TTL, id = Id }, R) ->
    {ok, Tx} =
        aeo_response_tx:new(#{oracle   => PK,
                              nonce    => N + 1,
                              query_id => Id,
                              response => R,
                              fee      => 2 + ttl_fee(TTL)}),
    Tx.

ttl_fee(_) -> 1.

%% -- State operations -------------------------------------------------------
update_oracle(O = #oracle{ address = PK }, S = #state{ oracles = Os }) ->
    S#state{ oracles = lists:keystore(PK, #oracle.address, Os, O) }.

update_query(Q = #query{ id = Id }, S = #state{ queries = Qs }) ->
    S#state{ queries = lists:keystore(Id, #query.id, Qs, Q) }.

add_response(Response, S) ->
    S#state{ responses = S#state.responses ++ [Response] }.

update_account(A, S = #state{ accounts = As }) ->
    S#state{ accounts = lists:keystore(A#account.pubkey, #account.pubkey, As, A) }.

non_oracle_accounts(#state{ accounts = As, oracles = Os, height = H }) ->
    lists:filter(fun(#account{ pubkey = PK }) ->
                    case lists:keyfind(PK, #oracle.address, Os) of
                        false -> true;
                        O     -> oracle_expired(O, H)
                    end
                 end, As).

balance(PK, #state{ accounts = As }) ->
    #account{ balance = B } = lists:keyfind(PK, #account.pubkey, As),
    B.

%% -- State service ----------------------------------------------------------
-define(SERVER, epoch_eqc).

state_start(Chain) ->
    (catch erlang:exit(whereis(?SERVER), kill)),
    timer:sleep(1),
    register(?SERVER, spawn(fun() -> loop(Chain) end)).

state_get() ->
    state_rpc(get).

state_put(Chain) ->
    state_rpc({put, Chain}).

state_rpc(Cmd) ->
    Ref = make_ref(),
    ?SERVER ! {epoch, self(), Ref, Cmd},
    receive
        {epoch, Ref, Res} -> Res
    after 200 ->
        error({rpc_timeout, Cmd})
    end.

loop(Chain) ->
    receive
        {epoch, From, Ref, get} ->
            From ! {epoch, Ref, Chain},
            loop(Chain);
        {epoch, From, Ref, {put, NewChain}} ->
            From ! {epoch, Ref, ok},
            loop(NewChain)
    end.

top_block_with_state(Chain) ->
    Block = aec_chain_state:top_block(Chain),
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    {ok, Trees} = aec_chain_state:get_block_state(BlockHash, Chain),
    {Block, Trees}.
