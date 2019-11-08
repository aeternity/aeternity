%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-

-record(aec_blocks             , {key, txs, pof}).
-record(aec_headers            , {key, value, height}).
-record(aec_call_state         , {key, value}).
-record(aec_contract_state     , {key, value}).
-record(aec_chain_state        , {key, value}).
-record(aec_block_state        , {key, value, difficulty, fork_id, fees, fraud}).
-record(aec_oracle_cache       , {key, value}).
-record(aec_oracle_state       , {key, value}).
-record(aec_account_state      , {key, value}).
-record(aec_channel_state      , {key, value}).
-record(aec_name_service_cache , {key, value}).
-record(aec_name_service_state , {key, value}).

-record(aec_signed_tx          , {key :: SignedTxHash :: aec_hash:hash(), value :: aetx_sign:signed_tx() | tuple()}).
-record(aec_tx_location        , {key :: SignedTxHash :: aec_hash:hash(), value :: BlockHash :: aec_hash:hash()}).
-record(aec_tx_pool            , {key :: SignedTxHash :: aec_hash:hash(), value :: PlaceHolder :: []}).
-record(aec_discovered_pof     , {key, value}).
-record(aec_signal_count       , {key :: KeyBlockHash :: aec_blocks:block_header_hash(), value :: SignalCount :: non_neg_integer()}).
