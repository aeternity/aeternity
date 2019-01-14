-module(aestratum_nonce).

-export([new/1,
         new/3,
         to_hex/1,
         to_int/1,
         to_int/3,
         max/1,
         merge/2,
         type/1,
         value/1,
         nbytes/1
        ]).

-export_type([nonce/0,
              hex_nonce/0,
              int_nonce/0,
              nbytes/0,
              part_nonce/0,
              part_type/0,
              part_int_nonce/0,
              part_nbytes/0
             ]).

-record(nonce, {
          value  :: int_nonce()
         }).

-record(part_nonce, {
          type   :: part_type(),
          value  :: part_int_nonce(),
          nbytes :: part_nbytes()
         }).

-define(MIN_NONCE, 0).
-define(MAX_NONCE, 16#ffffffffffffffff).

-define(NONCE_NBYTES, 8).

-define(MIN_PART_NONCE, 0).
-define(MAX_PART_NONCE_1, 16#ff).
-define(MAX_PART_NONCE_2, 16#ffff).
-define(MAX_PART_NONCE_3, 16#ffffff).
-define(MAX_PART_NONCE_4, 16#ffffffff).
-define(MAX_PART_NONCE_5, 16#ffffffffff).
-define(MAX_PART_NONCE_6, 16#ffffffffffff).
-define(MAX_PART_NONCE_7, 16#ffffffffffffff).

-define(MIN_PART_NONCE_NBYTES, 1).
-define(MAX_PART_NONCE_NBYTES, 7).

-define(IS_PART_NONCE_TYPE(Type), ((Type =:= extra) or (Type =:= miner))).
-define(IS_PART_NONCE_X(Nonce, Max), is_integer(Nonce) and
        ((Nonce >= ?MIN_PART_NONCE) and (Nonce =< Max))).

-define(IS_PART_NONCE(Type, Nonce, Max),
        ?IS_PART_NONCE_TYPE(Type) and ?IS_PART_NONCE_X(Nonce, Max)).

-opaque nonce()          :: #nonce{}.

-opaque part_nonce()     :: #part_nonce{}.

-type hex_nonce()        :: binary().

-type int_nonce()        :: ?MIN_NONCE..?MAX_NONCE.

-type nbytes()           :: ?NONCE_NBYTES.

-type part_type()        :: extra
                          | miner.

-type part_nbytes()      :: ?MIN_PART_NONCE_NBYTES..?MAX_PART_NONCE_NBYTES.

-type part_int_nonce()   :: part_int_nonce_1()
                          | part_int_nonce_2()
                          | part_int_nonce_3()
                          | part_int_nonce_4()
                          | part_int_nonce_5()
                          | part_int_nonce_6()
                          | part_int_nonce_7().

-type part_int_nonce_1() :: ?MIN_PART_NONCE..?MAX_PART_NONCE_1.

-type part_int_nonce_2() :: ?MIN_PART_NONCE..?MAX_PART_NONCE_2.

-type part_int_nonce_3() :: ?MIN_PART_NONCE..?MAX_PART_NONCE_3.

-type part_int_nonce_4() :: ?MIN_PART_NONCE..?MAX_PART_NONCE_4.

-type part_int_nonce_5() :: ?MIN_PART_NONCE..?MAX_PART_NONCE_5.

-type part_int_nonce_6() :: ?MIN_PART_NONCE..?MAX_PART_NONCE_6.

-type part_int_nonce_7() :: ?MIN_PART_NONCE..?MAX_PART_NONCE_7.

%% API.

-spec new(int_nonce()) -> nonce().
new(Nonce) when (Nonce >= 0) and (Nonce =< ?MAX_NONCE) ->
    #nonce{value = Nonce};
new(Nonce) ->
    erlang:error(badarg, [Nonce]).

-spec new(part_type(), part_int_nonce(), part_nbytes()) -> part_nonce().
new(Type, Nonce, 1) when ?IS_PART_NONCE(Type, Nonce, ?MAX_PART_NONCE_1) ->
    #part_nonce{type = Type, value = Nonce, nbytes = 1};
new(Type, Nonce, 2) when ?IS_PART_NONCE(Type, Nonce, ?MAX_PART_NONCE_2) ->
    #part_nonce{type = Type, value = Nonce, nbytes = 2};
new(Type, Nonce, 3) when ?IS_PART_NONCE(Type, Nonce, ?MAX_PART_NONCE_3) ->
    #part_nonce{type = Type, value = Nonce, nbytes = 3};
new(Type, Nonce, 4) when ?IS_PART_NONCE(Type, Nonce, ?MAX_PART_NONCE_4) ->
    #part_nonce{type = Type, value = Nonce, nbytes = 4};
new(Type, Nonce, 5) when ?IS_PART_NONCE(Type, Nonce, ?MAX_PART_NONCE_5) ->
    #part_nonce{type = Type, value = Nonce, nbytes = 5};
new(Type, Nonce, 6) when ?IS_PART_NONCE(Type, Nonce, ?MAX_PART_NONCE_6) ->
    #part_nonce{type = Type, value = Nonce, nbytes = 6};
new(Type, Nonce, 7) when ?IS_PART_NONCE(Type, Nonce, ?MAX_PART_NONCE_7) ->
    #part_nonce{type = Type, value = Nonce, nbytes = 7};
new(Type, Nonce, NBytes) ->
    erlang:error(badarg, [Type, Nonce, NBytes]).

-spec to_hex(nonce()) -> hex_nonce();
            (part_nonce()) -> hex_nonce().
to_hex(#nonce{value = Nonce}) ->
    to_hex1(Nonce, ?NONCE_NBYTES);
to_hex(#part_nonce{value = Nonce, nbytes = NBytes}) ->
    to_hex1(Nonce, NBytes);
to_hex(Nonce) ->
    erlang:error(badarg, [Nonce]).

-spec to_int(hex_nonce()) -> int_nonce().
to_int(Bin) when (byte_size(Bin) =:= (?NONCE_NBYTES * 2)) ->
    %% TODO: check is_hex
    to_int1(Bin);
to_int(Bin) ->
    erlang:error(badarg, [Bin]).

-spec to_int(part_type(), hex_nonce(), part_nbytes()) -> part_int_nonce().
to_int(Type, Bin, NBytes) when
      ?IS_PART_NONCE_TYPE(Type) and (byte_size(Bin) =:= (NBytes * 2)) and
      ((NBytes >= ?MIN_PART_NONCE_NBYTES) and
       (NBytes =< ?MAX_PART_NONCE_NBYTES)) ->
    %% TODO: check is_hex
    to_int1(Bin);
to_int(Type, Bin, NBytes) ->
    erlang:error(badarg, [Type, Bin, NBytes]).

-spec max(part_nbytes()) -> pos_integer().
max(1) -> ?MAX_PART_NONCE_1;
max(2) -> ?MAX_PART_NONCE_2;
max(3) -> ?MAX_PART_NONCE_3;
max(4) -> ?MAX_PART_NONCE_4;
max(5) -> ?MAX_PART_NONCE_5;
max(6) -> ?MAX_PART_NONCE_6;
max(7) -> ?MAX_PART_NONCE_7;
max(NBytes) -> erlang:error(badarg, [NBytes]).

-spec merge(part_nonce(), part_nonce()) -> nonce().
merge(#part_nonce{type = miner, value = Value1, nbytes = NBytes1},
      #part_nonce{type = extra, value = Value2, nbytes = NBytes2}) when
      (NBytes1 + NBytes2) =:= ?NONCE_NBYTES ->
    Bin = <<Value1:NBytes1/little-unit:8, Value2:NBytes2/little-unit:8>>,
    #nonce{value = binary:decode_unsigned(Bin, little)};
merge(#part_nonce{type = extra, value = Value1, nbytes = NBytes1},
      #part_nonce{type = miner, value = Value2, nbytes = NBytes2}) when
      (NBytes1 + NBytes2) =:= ?NONCE_NBYTES ->
    Bin = <<Value2:NBytes2/little-unit:8, Value1:NBytes1/little-unit:8>>,
    #nonce{value = binary:decode_unsigned(Bin, little)};
merge(PartNonce1, PartNonce2) ->
    erlang:error(badarg, [PartNonce1, PartNonce2]).

-spec type(part_nonce()) -> part_type().
type(#part_nonce{type = Type}) ->
    Type;
type(PartNonce) ->
    erlang:error(badarg, [PartNonce]).

-spec value(nonce()) -> int_nonce();
           (part_nonce()) -> part_int_nonce().
value(#nonce{value = Value}) ->
    Value;
value(#part_nonce{value = Value}) ->
    Value;
value(Nonce) ->
    erlang:error(badarg, [Nonce]).

-spec nbytes(nonce()) -> nbytes();
            (part_nonce()) -> part_nbytes().
nbytes(#nonce{}) ->
    ?NONCE_NBYTES;
nbytes(#part_nonce{nbytes = NBytes}) ->
    NBytes;
nbytes(PartNonce) ->
    erlang:error(badarg, [PartNonce]).

%% Internal functions.

to_hex1(Nonce, NBytes) ->
    <<begin
            if N < 10 -> <<($0 + N)>>;
               true   -> <<(87 + N)>>   %% 87 = ($a - 10)
            end
        end || <<N:4>> <= <<Nonce:NBytes/little-unit:8>>
    >>.

to_int1(Bin) ->
    binary_to_integer(
        list_to_binary(lists:reverse([X || <<X:2/binary>> <= Bin])), 16).

