-module(aesc_codec).

-export([enc/2,
         dec/1]).

-export([max_inband_msg_size/0]).

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
%% -type length()      :: i2bytes().
-type amount()      :: i8bytes().
-type pubkey()      :: bin32().
-type error_code()  :: i2bytes().

-define(PUBKEY_SIZE, 32).

enc(?CH_OPEN      , Msg) -> enc_ch_open(Msg);
enc(?CH_ACCEPT    , Msg) -> enc_ch_accept(Msg);
enc(?CH_REESTABL  , Msg) -> enc_ch_reestabl(Msg);
enc(?CH_REEST_ACK , Msg) -> enc_ch_reestabl_ack(Msg);
enc(?FND_CREATED  , Msg) -> enc_fnd_created(Msg);
enc(?FND_SIGNED   , Msg) -> enc_fnd_signed(Msg);
enc(?FND_LOCKED   , Msg) -> enc_fnd_locked(Msg);
enc(?UPDATE       , Msg) -> enc_update(Msg);
enc(?UPDATE_ACK   , Msg) -> enc_update_ack(Msg);
enc(?UPDATE_ERR   , Msg) -> enc_update_err(Msg);
enc(?DEP_CREATED  , Msg) -> enc_dep_created(Msg);
enc(?DEP_SIGNED   , Msg) -> enc_dep_signed(Msg);
enc(?DEP_LOCKED   , Msg) -> enc_dep_locked(Msg);
enc(?DEP_ERR      , Msg) -> enc_dep_err(Msg);
enc(?WDRAW_CREATED, Msg) -> enc_wdraw_created(Msg);
enc(?WDRAW_SIGNED , Msg) -> enc_wdraw_signed(Msg);
enc(?WDRAW_LOCKED , Msg) -> enc_wdraw_locked(Msg);
enc(?WDRAW_ERR    , Msg) -> enc_wdraw_err(Msg);
enc(?ERROR        , Msg) -> enc_error(Msg);
enc(?LEAVE        , Msg) -> enc_leave(Msg);
enc(?LEAVE_ACK    , Msg) -> enc_leave_ack(Msg);
enc(?SHUTDOWN     , Msg) -> enc_shutdown(Msg);
enc(?SHUTDOWN_ACK , Msg) -> enc_shutdown_ack(Msg);
enc(?INBAND_MSG   , Msg) -> enc_inband_msg(Msg).

-define(c(C), C:1/unit:8).

dec(<<?c(?ID_CH_OPEN)      , B/bytes>>) -> {?CH_OPEN     , dec_ch_open(B)};
dec(<<?c(?ID_CH_ACCEPT)    , B/bytes>>) -> {?CH_ACCEPT   , dec_ch_accept(B)};
dec(<<?c(?ID_CH_REESTABL)  , B/bytes>>) -> {?CH_REESTABL , dec_ch_reestabl(B)};
dec(<<?c(?ID_CH_REEST_ACK) , B/bytes>>) -> {?CH_REEST_ACK, dec_ch_reest_ack(B)};
dec(<<?c(?ID_FND_CREATED)  , B/bytes>>) -> {?FND_CREATED , dec_fnd_created(B)};
dec(<<?c(?ID_FND_SIGNED)   , B/bytes>>) -> {?FND_SIGNED  , dec_fnd_signed(B)};
dec(<<?c(?ID_FND_LOCKED)   , B/bytes>>) -> {?FND_LOCKED  , dec_fnd_locked(B)};
dec(<<?c(?ID_UPDATE)       , B/bytes>>) -> {?UPDATE      , dec_update(B)};
dec(<<?c(?ID_UPDATE_ACK)   , B/bytes>>) -> {?UPDATE_ACK  , dec_update_ack(B)};
dec(<<?c(?ID_UPDATE_ERR)   , B/bytes>>) -> {?UPDATE_ERR  , dec_update_err(B)};
dec(<<?c(?ID_DEP_CREATED)  , B/bytes>>) -> {?DEP_CREATED , dec_dep_created(B)};
dec(<<?c(?ID_DEP_SIGNED)   , B/bytes>>) -> {?DEP_SIGNED  , dec_dep_signed(B)};
dec(<<?c(?ID_DEP_LOCKED)   , B/bytes>>) -> {?DEP_LOCKED  , dec_dep_locked(B)};
dec(<<?c(?ID_DEP_ERR)      , B/bytes>>) -> {?DEP_ERR     , dec_dep_err(B)};
dec(<<?c(?ID_WDRAW_CREATED), B/bytes>>) -> {?WDRAW_CREATED, dec_wdraw_created(B)};
dec(<<?c(?ID_WDRAW_SIGNED) , B/bytes>>) -> {?WDRAW_SIGNED, dec_wdraw_signed(B)};
dec(<<?c(?ID_WDRAW_LOCKED) , B/bytes>>) -> {?WDRAW_LOCKED, dec_wdraw_locked(B)};
dec(<<?c(?ID_WDRAW_ERR)    , B/bytes>>) -> {?WDRAW_ERR   , dec_wdraw_err(B)};
dec(<<?c(?ID_ERROR)        , B/bytes>>) -> {?ERROR       , dec_error(B)};
dec(<<?c(?ID_LEAVE)        , B/bytes>>) -> {?LEAVE       , dec_leave(B)};
dec(<<?c(?ID_LEAVE_ACK)    , B/bytes>>) -> {?LEAVE_ACK   , dec_leave_ack(B)};
dec(<<?c(?ID_SHUTDOWN)     , B/bytes>>) -> {?SHUTDOWN    , dec_shutdown(B)};
dec(<<?c(?ID_SHUTDOWN_ACK) , B/bytes>>) -> {?SHUTDOWN_ACK, dec_shutdown_ack(B)};
dec(<<?c(?ID_INBAND_MSG)   , B/bytes>>) -> {?INBAND_MSG  , dec_inband_msg(B)}.

-type ch_open_msg() :: #{chain_hash           := hash()
                       , temporary_channel_id := chan_id()
                       , lock_period          := lock_period()
                       , push_amount          := amount()
                       , initiator_amount     := amount()
                       , responder_amount     := amount()
                       , channel_reserve      := amount()
                       , initiator            := pubkey()}.

-spec enc_ch_open(ch_open_msg()) -> binary().
enc_ch_open(#{chain_hash := ChainHash
            , temporary_channel_id := ChanId
            , lock_period          := LockPeriod
            , push_amount          := PushAmt
            , initiator_amount     := InitiatorAmt
            , responder_amount     := ResponderAmt
            , channel_reserve      := ChanReserve
            , initiator            := InitiatorPubkey}) ->
    << ?ID_CH_OPEN    :1 /unit:8
     , ChainHash      :32/binary
     , ChanId         :32/binary
     , LockPeriod     :2 /unit:8
     , PushAmt        :8 /unit:8
     , InitiatorAmt   :8 /unit:8
     , ResponderAmt   :8 /unit:8
     , ChanReserve    :8 /unit:8
     , InitiatorPubkey:32/binary >>.

-spec dec_ch_open(binary()) -> ch_open_msg().
dec_ch_open(<< ChainHash      :32/binary
             , ChanId         :32/binary
             , LockPeriod     :2 /unit:8
             , PushAmt        :8 /unit:8
             , InitiatorAmt   :8 /unit:8
             , ResponderAmt   :8 /unit:8
             , ChanReserve    :8 /unit:8
             , InitiatorPubkey:32/binary >>) ->
    #{ chain_hash           => ChainHash
     , temporary_channel_id => ChanId
     , lock_period          => LockPeriod
     , push_amount          => PushAmt
     , initiator_amount     => InitiatorAmt
     , responder_amount     => ResponderAmt
     , channel_reserve      => ChanReserve
     , initiator            => InitiatorPubkey}.


-type ch_accept_msg() :: #{ chain_hash           := hash()
                          , temporary_channel_id := chan_id()
                          , minimum_depth        := depth()
                          , initiator_amount     := amount()
                          , responder_amount     := amount()
                          , channel_reserve      := amount()
                          , responder            := pubkey()}.

-spec enc_ch_accept(ch_accept_msg()) -> binary().
enc_ch_accept(#{ chain_hash           := ChainHash
               , temporary_channel_id := ChanId
               , minimum_depth        := MinDepth
               , initiator_amount     := InitiatorAmt
               , responder_amount     := ResponderAmt
               , channel_reserve      := ChanReserve
               , responder            := Responder}) ->
    << ?ID_CH_ACCEPT  :1 /unit:8
     , ChainHash      :32/binary
     , ChanId         :32/binary
     , MinDepth       :4 /unit:8
     , InitiatorAmt   :8 /unit:8
     , ResponderAmt   :8 /unit:8
     , ChanReserve    :8 /unit:8
     , Responder      :32/binary >>.

-spec dec_ch_accept(binary()) -> ch_accept_msg().
dec_ch_accept(<< ChainHash      :32/binary
               , ChanId         :32/binary
               , MinDepth       :4/unit:8
               , InitiatorAmt   :8 /unit:8
               , ResponderAmt   :8 /unit:8
               , ChanReserve    :8 /unit:8
               , Responder      :32/binary >>) ->
    #{ chain_hash           => ChainHash
     , temporary_channel_id => ChanId
     , minimum_depth        => MinDepth
     , initiator_amount     => InitiatorAmt
     , responder_amount     => ResponderAmt
     , channel_reserve      => ChanReserve
     , responder            => Responder}.


-type ch_reestabl_msg() :: #{ chain_hash := hash()
                            , channel_id := chan_id()
                            , data := binary() }.

-spec enc_ch_reestabl(ch_reestabl_msg()) -> binary().
enc_ch_reestabl(#{ chain_hash := ChainHash
                 , channel_id := ChanId
                 , data       := Data }) ->
    EncData = encode_data(Data),
    << ?ID_CH_REESTABL:1 /unit:8
     , ChainHash      :32/binary
     , ChanId         :32/binary
     , EncData        /bytes>>.

-spec dec_ch_reestabl(binary()) -> ch_reestabl_msg().
dec_ch_reestabl(<< ChainHash:32/binary
                 , ChanId   :32/binary
                 , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ chain_hash => ChainHash
     , channel_id => ChanId
     , data       => Data }.


-type ch_reestabl_ack_msg() :: #{ chain_hash := hash()
                                , channel_id := chan_id()
                                , data := binary() }.

-spec enc_ch_reestabl_ack(ch_reestabl_ack_msg()) -> binary().
enc_ch_reestabl_ack(#{ chain_hash := ChainHash
                     , channel_id := ChanId
                     , data       := Data }) ->
    EncData = encode_data(Data),
    << ?ID_CH_REEST_ACK:1 /unit:8
     , ChainHash       :32/binary
     , ChanId          :32/binary
     , EncData        /bytes>>.

-spec dec_ch_reest_ack(binary()) -> ch_reestabl_ack_msg().
dec_ch_reest_ack(<< ChainHash:32/binary
                  , ChanId   :32/binary
                  , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ chain_hash => ChainHash
     , channel_id => ChanId
     , data       => Data }.


-type fnd_created_msg() :: #{ temporary_channel_id := chan_id()
                            , block_hash           := binary()
                            , data                 := binary()}.

-spec enc_fnd_created(fnd_created_msg()) -> binary().
enc_fnd_created(#{ temporary_channel_id := ChanId
                 , block_hash           := BlockHash
                 , data                 := Data }) ->
    EncData = encode_data(Data),
    << ?ID_FND_CREATED:1 /unit:8
     , ChanId         :32/binary
     , BlockHash      :32/binary
     , EncData        /bytes >>.

-spec dec_fnd_created(binary()) -> fnd_created_msg().
dec_fnd_created(<< ChanId:32/binary
                 , BlockHash:32/binary
                 , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ temporary_channel_id => ChanId
     , block_hash           => BlockHash
     , data                 => Data}.

-type fnd_signed_msg() :: #{ temporary_channel_id := chan_id()
                           , block_hash           := binary()
                           , data                 := binary()}.

-spec enc_fnd_signed(fnd_signed_msg()) -> binary().
enc_fnd_signed(#{temporary_channel_id := ChanId,
                 block_hash           := BlockHash,
                 data                 := Data}) ->
    EncData = encode_data(Data),
    << ?ID_FND_SIGNED:1 /unit:8
     , ChanId        :32/binary
     , BlockHash     :32/binary
     , EncData        /bytes >>.

-spec dec_fnd_signed(binary()) -> fnd_signed_msg().
dec_fnd_signed(<< ChanId:32/binary
                , BlockHash:32/binary
                , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ temporary_channel_id => ChanId
     , block_hash           => BlockHash
     , data                 => Data}.

-type fnd_locked_msg() :: #{ temporary_channel_id := chan_id()
                           , channel_id           := chan_id()}.

-spec enc_fnd_locked(fnd_locked_msg()) -> binary().
enc_fnd_locked(#{ temporary_channel_id := ChanId
                , channel_id           := OnChainId }) ->
    << ?ID_FND_LOCKED:1 /unit:8
     , ChanId        :32/binary
     , OnChainId     :32/binary >>.

-spec dec_fnd_locked(binary()) -> fnd_locked_msg().
dec_fnd_locked(<< ChanId:32/binary
                , OnChainId:32/binary >>) ->
    #{ temporary_channel_id => ChanId
     , channel_id           => OnChainId }.

-type update_msg() :: #{ channel_id := chan_id()
                       , block_hash := binary()
                       , data       := binary()}.
-spec enc_update(update_msg()) -> binary().
enc_update(#{ channel_id := ChanId
            , block_hash := BlockHash
            , data   := Data }) ->
    EncData = encode_data(Data),
    << ?ID_UPDATE:1 /unit:8
     , ChanId    :32/binary
     , BlockHash :32/binary
     , EncData   /bytes >>.

-spec dec_update(binary()) -> update_msg().
dec_update(<< ChanId:32/binary
            , BlockHash :32/binary
            , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ channel_id => ChanId
     , block_hash => BlockHash
     , data   => Data }.

-type update_ack_msg() :: #{ channel_id := chan_id()
                           , data       := binary()}.
-spec enc_update_ack(update_ack_msg()) -> binary().
enc_update_ack(#{ channel_id := ChanId
                , data       := Data }) ->
    EncData = encode_data(Data),
    << ?ID_UPDATE_ACK:1 /unit:8
     , ChanId        :32/binary
     , EncData       /bytes >>.

-spec dec_update_ack(binary()) -> update_ack_msg().
dec_update_ack(<< ChanId:32/binary
                , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ channel_id => ChanId
     , data   => Data }.

-type update_err_msg() :: #{ channel_id := chan_id()
                           , round      := non_neg_integer()
                           , error_code := error_code() }.

-spec enc_update_err(update_err_msg()) -> binary().
enc_update_err(#{ channel_id := ChanId
                , round      := Round
                , error_code := ErrCode }) ->
    << ?ID_UPDATE_ERR:1 /unit:8
     , ChanId        :32/binary
     , Round         :4 /unit:8
     , ErrCode       :2 /unit:8 >>.

-spec dec_update_err(binary()) -> update_err_msg().
dec_update_err(<< ChanId :32/binary
                , Round  :4/unit:8
                , ErrCode:2/unit:8 >>) ->
    #{ channel_id => ChanId
     , round      => Round
     , error_code => ErrCode }.

-type deposit_msg() :: #{ channel_id := chan_id()
                        , block_hash := binary()
                        , data       := binary()}.

-spec enc_dep_created(deposit_msg()) -> binary().
enc_dep_created(#{ channel_id := ChanId
                 , block_hash := BlockHash
                 , data       := Data }) ->
    EncData = encode_data(Data),
    << ?ID_DEP_CREATED:1 /unit:8
     , ChanId         :32/binary
     , BlockHash      :32/binary
     , EncData/bytes >>.

-spec dec_dep_created(binary()) -> deposit_msg().
dec_dep_created(<< ChanId:32/binary
                 , BlockHash:32/binary
                 , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ channel_id => ChanId
     , block_hash => BlockHash
     , data       => Data }.

-spec enc_dep_signed(deposit_msg()) -> binary().
enc_dep_signed(#{ channel_id := ChanId
                , block_hash := BlockHash
                , data       := Data }) ->
    EncData = encode_data(Data),
    << ?ID_DEP_SIGNED:1 /unit:8
     , ChanId        :32/binary
     , BlockHash     :32/binary
     , EncData/bytes >>.

-spec dec_dep_signed(binary()) -> deposit_msg().
dec_dep_signed(<< ChanId:32/binary
                , BlockHash:32/binary
                , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ channel_id => ChanId
     , block_hash => BlockHash
     , data       => Data }.

-type dep_locked_msg() :: #{ channel_id := chan_id()
                           , data       := binary()}.
-spec enc_dep_locked(dep_locked_msg()) -> binary().
enc_dep_locked(#{ channel_id := ChanId
                , data       := Data }) ->
    EncData = encode_data(Data),
    << ?ID_DEP_LOCKED:1 /unit:8
     , ChanId        :32/binary
     , EncData/bytes >>.

-spec dec_dep_locked(binary()) -> dep_locked_msg().
dec_dep_locked(<< ChanId:32/binary
                , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ channel_id => ChanId
     , data       => Data }.

-type dep_err_msg() :: #{ channel_id := chan_id()
                        , round      := non_neg_integer()
                        , error_code := error_code() }.

-spec enc_dep_err(dep_err_msg()) -> binary().
enc_dep_err(#{ channel_id := ChanId
             , round      := Round
             , error_code := ErrCode }) ->
    << ?ID_DEP_ERR:1 /unit:8
     , ChanId        :32/binary
     , Round         :4 /unit:8
     , ErrCode       :2 /unit:8 >>.

-spec dec_dep_err(binary()) -> dep_err_msg().
dec_dep_err(<< ChanId :32/binary
             , Round  :4 /unit:8
             , ErrCode:2 /unit:8 >>) ->
    #{ channel_id => ChanId
     , round      => Round
     , error_code => ErrCode }.

-type withdrawal_msg() :: #{ channel_id := chan_id()
                           , block_hash := binary()
                           , data       := binary()}.

-spec enc_wdraw_created(withdrawal_msg()) -> binary().
enc_wdraw_created(#{ channel_id := ChanId
                   , block_hash := BlockHash
                   , data       := Data }) ->
    EncData = encode_data(Data),
    << ?ID_WDRAW_CREATED:1 /unit:8
     , ChanId           :32/binary
     , BlockHash        :32/binary
     , EncData/bytes >>.

-spec dec_wdraw_created(binary()) -> withdrawal_msg().
dec_wdraw_created(<< ChanId:32/binary
                   , BlockHash:32/binary
                   , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ channel_id => ChanId
     , block_hash => BlockHash
     , data       => Data }.

-spec enc_wdraw_signed(withdrawal_msg()) -> binary().
enc_wdraw_signed(#{ channel_id := ChanId
                  , block_hash := BlockHash
                  , data       := Data }) ->
    EncData = encode_data(Data),
    << ?ID_WDRAW_SIGNED:1 /unit:8
     , ChanId          :32/binary
     , BlockHash       :32/binary
     , EncData/bytes >>.

-spec dec_wdraw_signed(binary()) -> withdrawal_msg().
dec_wdraw_signed(<< ChanId:32/binary
                  , BlockHash:32/binary
                  , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ channel_id => ChanId
     , block_hash => BlockHash
     , data       => Data }.

-type wdraw_locked_msg() :: #{ channel_id := chan_id()
                             , data       := binary()}.
-spec enc_wdraw_locked(wdraw_locked_msg()) -> binary().
enc_wdraw_locked(#{ channel_id := ChanId
                  , data       := Data }) ->
    EncData = encode_data(Data),
    << ?ID_WDRAW_LOCKED:1 /unit:8
     , ChanId          :32/binary
     , EncData/bytes >>.

-spec dec_wdraw_locked(binary()) -> wdraw_locked_msg().
dec_wdraw_locked(<< ChanId:32/binary
                  , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ channel_id => ChanId
     , data       => Data }.

-type wdraw_err_msg() :: #{ channel_id := chan_id()
                          , round      := non_neg_integer()
                          , error_code := error_code() }.

-spec enc_wdraw_err(wdraw_err_msg()) -> binary().
enc_wdraw_err(#{ channel_id := ChanId
               , round      := Round
               , error_code := ErrCode }) ->
    << ?ID_WDRAW_ERR:1 /unit:8
     , ChanId       :32/binary
     , Round        :4 /unit:8
     , ErrCode      :2 /unit:8 >>.

-spec dec_wdraw_err(binary()) -> wdraw_err_msg().
dec_wdraw_err(<< ChanId :32/binary
               , Round  :4 /unit:8
               , ErrCode:2 /unit:8 >>) ->
    #{ channel_id => ChanId
     , round      => Round
     , error_code => ErrCode }.

-type error_msg() :: #{ channel_id := chan_id()
                      , data       := binary() }.

-spec enc_error(error_msg()) -> binary().
enc_error(#{ channel_id := ChanId
           , data       := Data }) ->
    EncData = encode_data(Data),
    << ?ID_ERROR :1 /unit:8
     , ChanId    :32/binary
     , EncData   /bytes >>.

-spec dec_error(binary()) -> error_msg().
dec_error(<< ChanId:32/binary
           , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ channel_id => ChanId
     , data       => Data }.

-type leave_msg() :: #{channel_id := chan_id()}.

-spec enc_leave(leave_msg()) -> binary().
enc_leave(#{channel_id := ChanId}) ->
    << ?ID_LEAVE:1 /unit:8
     , ChanId   :32/binary >>.

-spec dec_leave(binary()) -> leave_msg().
dec_leave(<< ChanId:32/binary >>) ->
    #{ channel_id => ChanId }.

-type leave_ack_msg() :: #{channel_id := chan_id()}.

-spec enc_leave_ack(leave_ack_msg()) -> binary().
enc_leave_ack(#{channel_id := ChanId}) ->
    << ?ID_LEAVE_ACK:1 /unit:8
     , ChanId       :32/binary >>.

-spec dec_leave_ack(binary()) -> leave_ack_msg().
dec_leave_ack(<< ChanId:32/binary >>) ->
    #{ channel_id => ChanId }.

-type shutdown_msg() :: #{channel_id := chan_id(),
                          block_hash := binary(),
                          data       := binary() }.

-spec enc_shutdown(shutdown_msg()) -> binary().
enc_shutdown(#{channel_id := ChanId,
               block_hash := BlockHash,
               data       := Data }) ->
    EncData = encode_data(Data),
    << ?ID_SHUTDOWN:1 /unit:8
     , ChanId      :32/binary
     , BlockHash   :32/binary
     , EncData     /bytes >>.

-spec dec_shutdown(binary()) -> shutdown_msg().
dec_shutdown(<< ChanId:32/binary
              , BlockHash:32/binary
              , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ channel_id => ChanId
     , block_hash => BlockHash
     , data       => Data }.

-type shutdown_ack_msg() :: #{channel_id := chan_id(),
                              block_hash := binary(),
                              data       := binary() }.

-spec enc_shutdown_ack(shutdown_ack_msg()) -> binary().
enc_shutdown_ack(#{channel_id := ChanId,
                   block_hash := BlockHash,
                   data       := Data }) ->
    EncData = encode_data(Data),
    << ?ID_SHUTDOWN_ACK:1 /unit:8
     , ChanId          :32/binary
     , BlockHash       :32/binary
     , EncData         /bytes >>.

-spec dec_shutdown_ack(binary()) -> shutdown_ack_msg().
dec_shutdown_ack(<< ChanId:32/binary
                  , BlockHash:32/binary
                  , EncData/binary >>) ->
    Data = decode_data(EncData),
    #{ channel_id => ChanId
     , block_hash => BlockHash
     , data       => Data }.

-type inband_msg() :: #{channel_id := chan_id(),
                        from       := binary(),
                        to         := binary(),
                        info       := binary()}.

max_inband_msg_size() -> 65000.
inband_msg_bytes() -> 2.


-spec enc_inband_msg(inband_msg()) -> binary().
enc_inband_msg(#{ channel_id := ChanId
                , from       := From
                , to         := To
                , info       := Info }) ->
    assert_max_length(?PUBKEY_SIZE, byte_size(From), from),
    assert_max_length(?PUBKEY_SIZE, byte_size(To), to),
    Length = byte_size(Info),
    assert_max_length(max_inband_msg_size(), Length, msg),
    Bytes = inband_msg_bytes(),
    << ?ID_INBAND_MSG:1 /unit:8
     , ChanId        :32/binary
     , From          :?PUBKEY_SIZE/binary
     , To            :?PUBKEY_SIZE/binary
     , Length        :Bytes /unit:8
     , Info          :Length/bytes >>.

dec_inband_msg(<< ChanId:32/binary
                , From  :?PUBKEY_SIZE/binary
                , To    :?PUBKEY_SIZE/binary
                , Rest/binary >>) ->
    Bytes = inband_msg_bytes(),
    <<Length:Bytes/unit:8, Info/binary>> = Rest,
    Length = byte_size(Info),
    #{ channel_id => ChanId
     , from       => From
     , to         => To
     , info       => Info }.


assert_max_length(Max, L, Field) ->
    if L > Max ->
            erlang:error({max_length_exceeded, Field});
       true ->
            ok
    end.

encode_data(Data) ->
    encode_data(Data, 2).

encode_data(Data, LengthBytes) ->
    Length = byte_size(Data),
    <<Length:LengthBytes/unit:8
    , Data  /bytes >>.

decode_data(Bin) ->
    decode_data(Bin, 2).

decode_data(Bin, LengthBytes) ->
    <<Length:LengthBytes/unit:8
    , Data  /bytes >> = Bin,
    Length = byte_size(Data),
    Data.
