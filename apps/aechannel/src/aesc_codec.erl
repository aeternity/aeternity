-module(aesc_codec).

-export([enc/2,
         dec/1]).

-export_types([ch_open_msg/0,
               ch_accept_msg/0,
               ch_reestabl_msg/0,
               fnd_created_msg/0,
               fnd_signed_msg/0,
               fnd_locked_msg/0,
               upd_deposit_msg/0,
               upd_withdrawal_msg/0,
               shutdown_msg/0]).

-export_types([hash/0,
               chan_id/0,
               lock_period/0,
               amount/0,
               pubkey/0]).

-include("aesc_codec.hrl").

-type bin32()   :: <<_:256>>.
-type i2bytes() :: 0 .. 16#FFFF.
-type i4bytes() :: 0 .. 16#FFFFffff.
-type i8bytes() :: 0 .. 16#FFFFffffFFFFffff.

-type hash()        :: bin32().
-type chan_id()     :: bin32().
-type lock_period() :: i2bytes().
-type depth()       :: i4bytes().
-type length()      :: i2bytes().
-type amount()      :: i8bytes().
-type pubkey()      :: bin32().

enc(?CH_OPEN     , Msg) -> enc_ch_open(Msg);
enc(?CH_ACCEPT   , Msg) -> enc_ch_accept(Msg);
enc(?CH_REESTABL , Msg) -> enc_ch_reestabl(Msg);
enc(?FND_CREATED , Msg) -> enc_fnd_created(Msg);
enc(?FND_SIGNED  , Msg) -> enc_fnd_signed(Msg);
enc(?FND_LOCKED  , Msg) -> enc_fnd_locked(Msg);
enc(?UPD_DEPOSIT , Msg) -> enc_upd_deposit(Msg);
enc(?UPD_WITHDRAW, Msg) -> enc_upd_withdraw(Msg);
enc(?SHUTDOWN    , Msg) -> enc_shutdown(Msg).

-define(id(C), C:1/unit:8).

dec(<<?id(?ID_CH_OPEN)     , B/bytes>>) -> {?CH_OPEN     , dec_ch_open(B)};
dec(<<?id(?ID_CH_ACCEPT)   , B/bytes>>) -> {?CH_ACCEPT   , dec_ch_accept(B)};
dec(<<?id(?ID_CH_REESTABL) , B/bytes>>) -> {?CH_REESTABL , dec_ch_reestabl(B)};
dec(<<?id(?ID_FND_CREATED) , B/bytes>>) -> {?FND_CREATED , dec_fnd_created(B)};
dec(<<?id(?ID_FND_SIGNED)  , B/bytes>>) -> {?FND_SIGNED  , dec_fnd_signed(B)};
dec(<<?id(?ID_FND_LOCKED)  , B/bytes>>) -> {?FND_LOCKED  , dec_fnd_locked(B)};
dec(<<?id(?ID_UPD_DEPOSIT) , B/bytes>>) -> {?UPD_DEPOSIT , dec_upd_deposit(B)};
dec(<<?id(?ID_UPD_WITHDRAW), B/bytes>>) -> {?UPD_WITHDRAW, dec_upd_withdraw(B)};
dec(<<?id(?ID_SHUTDOWN)    , B/bytes>>) -> {?SHUTDOWN    , dec_shutdown(B)}.


-type ch_open_msg() :: #{chain_hash           := hash()
                       , temporary_channel_id := chan_id()
                       , lock_period          := lock_period()
                       , push_amount          := amount()
                       , initiator_amount     := amount()
                       , participant_amount   := amount()
                       , channel_reserve      := amount()
                       , initiator            := pubkey()}.

-spec enc_ch_open(ch_open_msg()) -> binary().
enc_ch_open(#{chain_hash := ChainHash
            , temporary_channel_id := ChanId
            , lock_period          := LockPeriod
            , push_amount          := PushAmt
            , initiator_amount     := InitiatorAmt
            , participant_amount   := ParticipantAmt
            , channel_reserve      := ChanReserve
            , initiator            := InitiatorPubkey}) ->
    << ?ID_CH_OPEN    :1 /unit:8
     , ChainHash      :32/binary
     , ChanId         :32/binary
     , LockPeriod     :2 /unit:8
     , PushAmt        :8 /unit:8
     , InitiatorAmt   :8 /unit:8
     , ParticipantAmt :8 /unit:8
     , ChanReserve    :8 /unit:8
     , InitiatorPubkey:32/binary >>.

-spec dec_ch_open(binary()) -> ch_open_msg().
dec_ch_open(<< ChainHash      :32/binary
             , ChanId         :32/binary
             , LockPeriod     :2 /unit:8
             , PushAmt        :8 /unit:8
             , InitiatorAmt   :8 /unit:8
             , ParticipantAmt :8 /unit:8
             , ChanReserve    :8 /unit:8
             , InitiatorPubkey:32/binary >>) ->
    #{chain_hash           => ChainHash
    , temporary_channel_id => ChanId
    , lock_period          => LockPeriod
    , push_amount          => PushAmt
    , initiator_amount     => InitiatorAmt
    , participant_amount   => ParticipantAmt
    , channel_reserve      => ChanReserve
    , initiator            => InitiatorPubkey}.


-type ch_accept_msg() :: #{temporary_channel_id := chan_id()
                         , chain_hash           := hash()
                         , minimum_depth        := depth()
                         , initiator_amount     := amount()
                         , participant_amount   := amount()
                         , channel_reserve      := amount()
                         , initiator            := pubkey()}.

-spec enc_ch_accept(ch_accept_msg()) -> binary().
enc_ch_accept(#{temporary_channel_id := ChanId
              , chain_hash           := ChainHash
              , minimum_depth        := MinDepth
              , initiator_amount     := InitiatorAmt
              , participant_amount   := ParticipantAmt
              , channel_reserve      := ChanReserve
              , initiator            := InitiatorPubkey}) ->
    << ?ID_CH_ACCEPT  :1 /unit:8
     , ChanId         :32/binary
     , ChainHash      :32/binary
     , MinDepth       :4 /unit:8
     , InitiatorAmt   :8 /unit:8
     , ParticipantAmt :8 /unit:8
     , ChanReserve    :8 /unit:8
     , InitiatorPubkey:32/binary >>.

-spec dec_ch_accept(binary()) -> ch_accept_msg().
dec_ch_accept(<< ChanId         :32/binary
               , ChainHash      :32/binary
               , MinDepth       :4/unit:8
               , InitiatorAmt   :8 /unit:8
               , ParticipantAmt :8 /unit:8
               , ChanReserve    :8 /unit:8
               , InitiatorPubkey:32/binary >>) ->
    #{temporary_channel_id => ChanId
    , chain_hash           => ChainHash
    , minimum_depth        => MinDepth
    , initiator_amount     => InitiatorAmt
    , participant_amount   => ParticipantAmt
    , channel_reserve      => ChanReserve
    , initiator            => InitiatorPubkey}.


-type ch_reestabl_msg() :: #{temporary_channel_id := chan_id()}.

-spec enc_ch_reestabl(ch_reestabl_msg()) -> binary().
enc_ch_reestabl(#{temporary_channel_id := ChanId}) ->
    << ?ID_CH_REESTABL:1 /unit:8
     , ChanId         :32/binary >>.

-spec dec_ch_reestabl(binary()) -> ch_reestabl_msg().
dec_ch_reestabl(<< ChanId:32/binary >>) ->
    #{temporary_channel_id => ChanId}.


-type fnd_created_msg() :: #{ temporary_channel_id := chan_id()
                            , data                 := binary()}.

-spec enc_fnd_created(fnd_created_msg()) -> binary().
enc_fnd_created(#{temporary_channel_id := ChanId,
                  data                 := Data}) ->
    Length = byte_size(Data),
    << ?ID_FND_CREATED:1 /unit:8
     , ChanId         :32/binary
     , Length         :2 /unit:8
     , Data           :Length/bytes >>.

-spec dec_fnd_created(binary()) -> fnd_created_msg().
dec_fnd_created(<< ChanId:32/binary
                 , Length:2/unit:8
                 , Data/binary >>) ->
    Length = byte_size(Data),
    #{ temporary_channel_id => ChanId
     , data                 => Data}.

-type fnd_signed_msg() :: #{ temporary_channel_id := chan_id()
                           , data                 := binary()}.

-spec enc_fnd_signed(fnd_signed_msg()) -> binary().
enc_fnd_signed(#{temporary_channel_id := ChanId,
                 data                 := Data}) ->
    Length = byte_size(Data),
    << ?ID_FND_SIGNED:1 /unit:8
     , ChanId        :32/binary
     , Length        :2 /unit:8
     , Data          :Length/bytes >>.

-spec dec_fnd_signed(binary()) -> fnd_signed_msg().
dec_fnd_signed(<< ChanId:32/binary
                , Length:2/unit:8
                , Data/binary >>) ->
    Length = byte_size(Data),
    #{ temporary_channel_id => ChanId
     , data                 => Data}.

-type fnd_locked_msg() :: #{temporary_channel_id := chan_id()}.

-spec enc_fnd_locked(fnd_locked_msg()) -> binary().
enc_fnd_locked(#{temporary_channel_id := ChanId}) ->
    << ?ID_FND_LOCKED:1 /unit:8
     , ChanId        :32/binary >>.

-spec dec_fnd_locked(binary()) -> fnd_locked_msg().
dec_fnd_locked(<< ChanId:32/binary >>) ->
    #{temporary_channel_id => ChanId}.


-type upd_deposit_msg() :: #{temporary_channel_id := chan_id()
                           , length               := length()
                           , data                 := binary()}.

-spec enc_upd_deposit(upd_deposit_msg()) -> binary().
enc_upd_deposit(#{temporary_channel_id := ChanId
                , length := Length
                , data   := Data}) ->
    Length = byte_size(Data),
    << ?ID_UPD_DEPOSIT:1 /unit:8
     , ChanId         :32/binary
     , Length         :2 /unit:8
     , Data           :Length/bytes >>.

-spec dec_upd_deposit(binary()) -> upd_deposit_msg().
dec_upd_deposit(<< ChanId:32/binary
                 , Length:2 /unit:8
                 , Data/bytes >>) ->
    Length = byte_size(Data),
    #{temporary_channel_id => ChanId
    , length => Length
    , data   => Data}.

-type upd_withdrawal_msg() :: #{temporary_channel_id := chan_id()
                              , length               := length()
                              , data                 := binary()}.

-spec enc_upd_withdraw(upd_withdrawal_msg()) -> binary().
enc_upd_withdraw(#{temporary_channel_id := ChanId
                 , length               := Length
                 , data                 := Data}) ->
    << ?ID_UPD_WITHDRAW :1 /unit:8
     , ChanId           :32/binary
     , Length           :2 /unit:8
     , Data             :Length/bytes >>.

-spec dec_upd_withdraw(binary()) -> upd_withdrawal_msg().
dec_upd_withdraw(<< ChanId:32/binary
                  , Length:2 /unit:8
                  , Data/bytes >>) ->
    Length = byte_size(Data),
    #{temporary_channel_id => ChanId
    , length => Length
    , data   => Data}.

-type shutdown_msg() :: #{temporary_channel_id := chan_id()}.

-spec enc_shutdown(shutdown_msg()) -> binary().
enc_shutdown(#{temporary_channel_id := ChanId}) ->
    << ?ID_SHUTDOWN:1 /unit:8
     , ChanId      :32/binary >>.

-spec dec_shutdown(binary()) -> shutdown_msg().
dec_shutdown(<< ChanId:32/binary >>) ->
    #{temporary_channel_id => ChanId}.
