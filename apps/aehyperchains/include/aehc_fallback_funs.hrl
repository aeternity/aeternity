%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-

-type fallback_funs() :: adjust_unmined_keyblock
    | assert_key_target_range
    | default_target
    | dirty_validate_block_pre_conductor
    | dirty_validate_header_pre_conductor
    | dirty_validate_key_hash_at_height
    | dirty_validate_key_node_with_ctx
    | dirty_validate_micro_node_with_ctx
    | generate_key_header_seal
    | keyblocks_for_target_calc
    | key_header_difficulty
    | key_header_for_sealing
    | next_nonce_for_sealing
    | nonce_for_sealing
    | recent_cache_n
    | recent_cache_trim_key_header
    | recent_cache_n
    | set_key_block_seal
    | start
    | state_grant_reward
    | trim_sealing_nonce
    | validate_key_header_seal.
