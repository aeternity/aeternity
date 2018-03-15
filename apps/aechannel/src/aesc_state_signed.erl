%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for signed local state objects
%%% @end
%%%=============================================================================
-module(aesc_state_signed).

%% API
-export([deserialize/1,
         serialize/1,
         sign_state/2,
         verify/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type signatures() :: ordsets:ordset(binary()).
-type priv_key()   :: binary().

-record(signed_state, {
          state      :: aesc_state:state(),
          signatures :: signatures()
         }).
-opaque signed_state() :: signed_state().

-export_type([signed_state/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec deserialize(binary()) -> signed_state().
deserialize(Bin) ->
    {ok, SignedState} = msgpack:unpack(Bin),
    #{<<"state">> := State,
      <<"sigs">>  := Sigs} = SignedState,
    #signed_state{state      = aesc_state:deserialize(State),
                  signatures = Sigs}.

-spec serialize(signed_state()) -> binary().
serialize(#signed_state{state = State, signatures = Sigs}) ->
    msgpack:pack(#{<<"sigs">>  => Sigs,
                   <<"state">> => aesc_state:serialize(State)}).

%% Perhaps reunion signing and verification with aecore tx signing/verification?
-spec sign_state(signed_state(), priv_key()) -> signed_state().
sign_state(#signed_state{state = State, signatures = Sigs} = S, PrivKey) ->
    Bin  = aesc_state:serialize_to_bin(State),
    Sign = crypto:sign(ecdsa, sha256, Bin, [PrivKey, crypto:ec_curve(secp256k1)]),
    S#signed_state{signatures = ordsets:add_element(Sign, Sigs)}.

-spec verify(signed_state()) -> boolean().
verify(#signed_state{state = State, signatures = Sigs}) ->
    Signers = aesc_state:pubkeys(State),
    Bin     = aesc_state:serialize_to_bin(State),
    lists:all(fun(Signer) -> verify(Signer, Sigs, Bin) end, Signers).

%%%===================================================================
%%% Internal functions
%%%===================================================================

verify(PubKey, Sigs, Bin) ->
    lists:any(
      fun(Sig) ->
              crypto:verify(ecdsa, sha256, Bin, Sig, [PubKey, crypto:ec_curve(secp256k1)])
      end, Sigs).
