%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Subname transaction
%%% @end
%%%=============================================================================

-module(aens_subname_tx).

-include("aens.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         signers/2,
         version/1,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1,
         valid_at_protocol/2
        ]).

%% Getters
-export([name/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(SUBNAME_TX_VSN, 1).
-define(SUBNAME_TX_TYPE, subname_tx).

-type pointers() :: [aens_pointer:pointer()].
-type subnames() :: [{SubName :: binary(), pointers()}].

-record(ns_subname_tx, {
          account_id :: aeser_id:id(),
          nonce      :: integer(),
          name       :: binary(),
          definition :: subnames(),
          fee        :: integer(),
          ttl        :: aetx:tx_ttl()
         }).

-opaque tx() :: #ns_subname_tx{}.

-export_type([tx/0,
              pointers/0,
              subnames/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account_id := AccountId,
      nonce      := Nonce,
      name_id    := NameId,
      definition := Definition,
      fee        := Fee} = Args) ->
    account = aeser_id:specialize_type(AccountId),
    {name, Name} = aeser_id:specialize(NameId),
    SubnameDefs =
        maps:fold(
          fun (SName, SPointers, Acc) ->
                  case aens_utils:top_name(SName) of
                      {ok, subname, Name} ->
                          {ok, SNameAscii} = aens_utils:ascii_encode(SName),
                          [{list_to_binary(SNameAscii),
                            pointers_list(SPointers)} | Acc];
                      _ ->
                          error({invalid_subname, SName})
                  end
          end, [], Definition),
    Tx = #ns_subname_tx{account_id = AccountId,
                        nonce      = Nonce,
                        name       = Name,
                        definition = SubnameDefs,
                        fee        = Fee,
                        ttl        = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.


-spec type() -> atom().
type() ->
    ?SUBNAME_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_subname_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#ns_subname_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ns_subname_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_subname_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ns_subname_tx{} = Tx) ->
    account_pubkey(Tx).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ns_subname_tx{} = _Tx, Trees,_Env) ->
    %% Checks in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#ns_subname_tx{} = Tx, Trees, Env) ->
    Instructions =
        aeprimop:subname_tx_instructions(
          account_pubkey(Tx),
          name(Tx),
          definition(Tx),
          fee(Tx),
          nonce(Tx)),
    aeprimop:eval(Instructions, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ns_subname_tx{} = Tx, _) ->
    {ok, [account_pubkey(Tx)]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_subname_tx{account_id = AccountId,
                         nonce      = Nonce,
                         name       = Name,
                         definition = Definition,
                         fee        = Fee,
                         ttl        = TTL} = Tx) ->
    {version(Tx),
     [ {account_id, AccountId}
     , {nonce, Nonce}
     , {name, Name}
     , {definition, map_pointers(fun aens_pointer:to_tuple/1, Definition)}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?SUBNAME_TX_VSN,
            [ {account_id, AccountId}
            , {nonce, Nonce}
            , {name, Name}
            , {definition, Definition}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    account = aeser_id:specialize_type(AccountId),
    #ns_subname_tx{account_id = AccountId,
                   nonce      = Nonce,
                   name       = Name,
                   definition = map_pointers(fun aens_pointer:new/1, Definition),
                   fee        = Fee,
                   ttl        = TTL}.

serialization_template(?SUBNAME_TX_VSN) ->
    [ {account_id, id}
    , {nonce, int}
    , {name, binary}
    , {definition, [{binary, [{binary, id}]}]}
    , {fee, int}
    , {ttl, int}
    ].

-spec for_client(tx()) -> map().
for_client(#ns_subname_tx{account_id = AccountId,
                         nonce      = Nonce,
                         name       = Name,
                         definition = Definition,
                         fee        = Fee,
                         ttl        = TTL}) ->
    #{<<"account_id">> => aeser_api_encoder:encode(id_hash, AccountId),
      <<"nonce">>      => Nonce,
      <<"name">>       => Name,
      <<"definition">> => map_pointers(fun aens_pointer:serialize_for_client/1, Definition),
      <<"fee">>        => Fee,
      <<"ttl">>        => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec name(tx()) -> binary().
name(#ns_subname_tx{name = Name}) ->
    Name.

-spec definition(tx()) -> subnames().
definition(#ns_subname_tx{definition = Definition}) ->
    Definition.

%%%===================================================================
%%% Internal functions
%%%===================================================================

account_pubkey(#ns_subname_tx{account_id = AccountId}) ->
    aeser_id:specialize(AccountId, account).

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?SUBNAME_TX_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(_, _) ->
    true.


map_pointers(Fun, Definition) ->
    [{Subname, lists:map(Fun, Pointers)} || {Subname, Pointers} <- Definition].

pointers_list(Pointers) when is_map(Pointers) ->
    maps:fold(fun (Key, Id, Acc) ->
                      [aens_pointer:new(Key, Id) | Acc]
              end, [], Pointers).
