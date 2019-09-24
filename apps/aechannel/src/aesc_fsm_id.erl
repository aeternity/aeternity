%%%=============================================================================
%%% @copyright 2019, Aeternity Anstalt
%%% @doc
%%%    Module wrapping the FSM ID - the ID is used as an encryption key for protecting the persisted
%%%    channel state. The ID is wrapped in a getter function - any attempt to print out
%%%    the ID will result in printing the atom representing the function.
%%% @end
%%%=============================================================================
-module(aesc_fsm_id).

-export([ new/0
        , from_binary/1
        , retrieve/1
        , retrieve_for_client/1
        , compare/2
        ]).

-opaque wrapper() :: fun(() -> binary()).
-export_type([wrapper/0]).

-define(FSM_ID_SIZE, 32).

-spec new() -> wrapper().
new() ->
    FsmId = crypto:strong_rand_bytes(?FSM_ID_SIZE),
    fun() -> FsmId end.

-spec from_binary(binary()) -> wrapper().
from_binary(FsmId) when size(FsmId) == ?FSM_ID_SIZE ->
    fun() -> FsmId end.

-spec retrieve(wrapper()) -> binary().
retrieve(FsmIdWrapper) -> FsmIdWrapper().

-spec retrieve_for_client(wrapper()) -> binary().
retrieve_for_client(FsmIdWrapper) ->
    aeser_api_encoder:encode(bytearray, FsmIdWrapper()).

-spec compare(wrapper(), wrapper()) -> boolean().
%% @doc
%%      The ID is used as a cryptographic key. For reconnecting to the FSM the key is used for
%%      authenticating the user. The user should NEVER compare the ID's directly - only by using
%%      this constant time wrapper in order to avoid timing attacks.
%% @end
compare(FsmIdWrapper1, FsmIdWrapper2) ->
    FsmId1 = retrieve(FsmIdWrapper1),
    FsmId2 = retrieve(FsmIdWrapper2),
    enacl:verify_32(FsmId1, FsmId2).
