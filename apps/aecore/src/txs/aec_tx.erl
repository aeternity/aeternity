-module(aec_tx).

-export([fee/1,
         apply_signed/3]).
-export([serialize/1,
         deserialize/1,
         serialize_to_binary/1,
         deserialize_from_binary/1]).

-include("common.hrl").
-include("trees.hrl").
-include("txs.hrl").

-export_type([tx/0,
              signed_tx/0]).

%%%=============================================================================
%%% aec_tx behavior callbacks
%%%=============================================================================

-callback new(Args :: map(), Trees :: trees()) ->
    {ok, Tx :: term()} | {error, Reason :: term()}.

-callback check(Tx :: term(), Trees :: trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: trees()} | {error, Reason :: term()}.

-callback process(Tx :: term(), Trees :: trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: trees()}.


%% Relax type spec for now to have different spec in coinbase/spend
-callback serialize(Tx :: term()) -> term().

%% Relax type spec for now to have different spec in coinbase/spend
-callback deserialize(term()) -> Tx :: term().

-callback type() -> binary().

%%%%=============================================================================
%% API
%%%=============================================================================

fee(#coinbase_tx{}) ->
    0;
fee(#spend_tx{fee = F}) ->
    F.

-spec apply_signed(list(signed_tx()), trees(), non_neg_integer()) ->
                          {ok, trees()} | {error, term()}.
apply_signed([], Trees, _Height) ->
    {ok, Trees};
apply_signed([SignedTx | Rest], Trees0, Height) ->
    case aec_tx_sign:verify(SignedTx) of
        ok ->
            Tx = aec_tx_sign:data(SignedTx),
            case check_single(Tx, Trees0, Height) of
                {ok, Trees1} ->
                    case process_single(Tx, Trees1, Height) of
                        {ok, Trees2} ->
                            apply_signed(Rest, Trees2, Height);
                        {error, _Reason} = Error ->
                            Error
                    end;
                {error, _} = CheckError ->
                    CheckError
            end;
        {error, _Reason} = Error ->
            Error
    end.

serialize(Tx) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:serialize(Tx).

deserialize(Data) ->
    Mod = tx_dispatcher:handler_by_type(type_of(Data)),
    Mod:deserialize(Data).

serialize_to_binary(Tx) ->
    msgpack:pack(serialize(Tx)).

deserialize_from_binary(Bin) ->
    {ok, Unpacked} = msgpack:unpack(Bin),
    deserialize(Unpacked).

type_of([#{}|_] = L) ->
    [Type] = [T || #{<<"type">> := T} <- L],
    Type;
type_of([Type|_]) ->
    Type.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Check transaction. Prepare state tree: e.g., create newly referenced account
%%------------------------------------------------------------------------------
-spec check_single(tx(), trees(), non_neg_integer()) -> {ok, trees()} | {error, term()}.
check_single(Tx, Trees, Height) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:check(Tx, Trees, Height).

%%------------------------------------------------------------------------------
%% Process the transaction. Accounts must already be present in the state tree
%%------------------------------------------------------------------------------
-spec process_single(tx(), trees(), non_neg_integer()) -> {ok, trees()}.
process_single(Tx, Trees, Height) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:process(Tx, Trees, Height).
