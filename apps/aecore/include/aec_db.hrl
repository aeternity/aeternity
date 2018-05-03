%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-

-record(aec_blocks             , {key, txs}).
-record(aec_headers            , {key, value, height}).
-record(aec_contract_state     , {key, value}).
-record(aec_tx                 , {key, tx}).
-record(aec_chain_state        , {key, value}).
-record(aec_block_state        , {key, value, difficulty, fork_id}).
-record(aec_oracle_cache       , {key, value}).
-record(aec_oracle_state       , {key, value}).
-record(aec_account_state      , {key, value}).
-record(aec_channel_state      , {key, value}).
-record(aec_name_service_cache , {key, value}).
-record(aec_name_service_state , {key, value}).
