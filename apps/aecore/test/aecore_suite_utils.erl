-module(aecore_suite_utils).

%% Common Test configuration functions.
-export([init_per_suite/2,
         init_per_suite/3,
         init_per_suite/4]).

-export([top_dir/1,
         priv_dir/1,
         make_shortcut/1,
         node_config/2,
         create_config/4,
         create_seed_file/2,
         hard_fork_filename/4,
         make_multi/2,
         node_shortcut/2,
         shortcut_dir/1,
         symlink/2]).

-export([cmd/3, cmd/4, cmd/5, cmd/6,
         cmd_res/1,
         delete_file/1,
         set_env/4,
         unset_env/3]).

-export([start_node/2,  % (N, Config)
         start_node/3,  % (N, Config, ExtraEnv :: [{OSEnvVarName, Value}])

         stop_node/2,
         reinit_with_bitcoin_ng/1,
         reinit_with_ct_consensus/1,
         reinit_nodes_with_ct_consensus/1,
         get_node_db_config/1,
         delete_node_db_if_persisted/1,
         expected_mine_rate/0,
         hc_mine_blocks/2,
         hc_mine_blocks/4,
         mine_blocks/2,
         mine_blocks/3,
         mine_blocks/4,
         mine_blocks/5,
         mine_all_txs/2,
         mine_micro_block_emptying_mempool_or_fail/1,
         mine_blocks_until_txs_on_chain/3,
         mine_blocks_until_txs_on_chain/4,
         mine_blocks_until_txs_on_chain/5,
         mine_key_blocks/2,
         mine_key_blocks/3,
         mine_micro_blocks/2,
         mine_micro_blocks/3,
         get_key_hash_by_delta/2,
         wait_for_tx_in_pool/2,
         wait_for_tx_in_pool/3,
         wait_for_height/2,
         wait_for_height/3,
         flush_new_blocks/0,
         flush_mempool/2,
         spend/5,         %% (Node, FromPub, ToPub, Amount, Fee) -> ok
         sign_on_node/2,
         sign_on_node/3,
         forks/0,
         latest_fork_height/0,
         latest_protocol_version/0]).

-export([mock_mempool_nonce_offset/2,
         unmock_mempool_nonce_offset/1]).

-export([node_tuple/1,
         node_name/1,
         parent_chain_node/1,
         external_api_port/1,

         peer_info/1,
         connect/1,
         connect/2,
         connect_wait/2,
         connect_await_mode/1,
         subscribe/2,
         unsubscribe/2,
         events_since/3,
         all_events_since/2,
         check_for_logs/2,
         times_in_epoch_log/3,
         errors_in_logs/2,
         assert_no_errors_in_logs/1,
         assert_no_errors_in_logs/2
        ]).

-export([proxy/0,
         start_proxy/0,
         call_proxy/2,
         await_aehttp/1,
         await_sync_complete/2,
         await_is_syncing/2,
         rpc/3,
         rpc/4,
         use_api/1,
         http_request/4,
         httpc_request/4,
         http_api_prefix/0,
         process_http_return/1,
         internal_address/0,
         external_address/0,
         rosetta_address/0,
         rosetta_offline_address/0,
         block_peer/2,
         unblock_peer/2
        ]).

-export([generate_key_pair/0]).

-export([restart_jobs_server/1]).

-export([patron/0,
         sign_keys/1,
         meta_tx/4]).

%% Mocking helpers for tests
-export([start_mocks/2,
         start_mock/2,
         end_mocks/2,
         end_mock/2]).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(OPS_BIN, "aeternity").
-define(DEFAULT_CUSTOM_EXPECTED_MINE_RATE, 100).
-define(DEFAULT_NODE, dev1).
-define(MAX_NODES_PER_GROUP, 9).

-record(mock_def, { module     :: atom()
                  , init_fun   :: atom()
                  , finish_fun :: atom()}).

%% Keys for P2P communication
static_peer_keys() ->
    #{dev1 =>
          {<<120,30,108,92,13,32,45,162,66,181,135,13,102,186,226,7,
            134,64,127,57,44,122,62,198,148,18,128,51,162,218,180,
            97>>,
          <<128,38,224,217,226,249,89,153,69,120,34,192,93,224,163,
            234,105,76,186,215,58,166,69,75,31,103,31,243,148,225,
            253,127>>}, %% pp_ySU7cHqsymnuBP9iSe4rMnH1Rz2FStx5rnoewYMJcuPhdaqPk
      dev2 =>
          {<<112,66,119,236,84,180,214,104,63,254,231,93,110,189,
            200,155,126,48,77,78,163,89,198,71,59,16,145,112,73,
            249,93,91>>,
          <<137,121,210,193,164,178,71,99,63,76,25,128,199,153,210,
            37,125,233,17,162,151,39,188,155,185,197,70,250,93,44,
            83,52>>}, %% pp_23YdvfRPQ1b1AMWmkKZUGk2cQLqygQp55FzDWZSEUicPjhxtp5
      dev3 =>
          {<<192,145,22,50,217,175,73,12,42,218,16,92,216,240,151,
            252,189,80,190,47,62,203,178,89,230,75,253,78,114,65,
            96,78>>,
          <<177,115,250,203,226,39,102,92,8,182,166,254,125,117,
            140,134,199,149,211,182,184,107,119,43,218,70,251,60,
            10,56,12,53>>}, %% pp_2M9oPohzsWgJrBBCFeYi3PVT4YF7F2botBtq6J1EGcVkiutx3R
      dev4 =>
          {<<8,4,61,78,239,59,15,114,60,237,94,37,99,234,77,171,196,
            50,174,62,211,238,187,245,77,129,9,142,194,142,220,87>>,
          <<92,250,64,149,14,18,50,103,129,68,139,81,60,184,196,
            165,24,69,92,154,213,20,83,120,32,253,56,243,84,205,
            175,25>>},
      dev5 =>
          {<<120,191,135,183,37,65,25,244,20,102,185,177,240,197,
            193,138,80,250,22,98,226,78,132,86,42,233,54,159,141,
            152,240,92>>,
          <<236,79,122,42,231,4,219,78,111,26,16,14,188,79,195,66,
            132,170,184,174,133,239,89,92,42,131,239,135,55,116,
            143,91>>},
      dev6 =>
          {<<104,152,176,222,74,34,189,188,125,190,42,90,196,148,
            220,214,178,80,89,193,82,194,4,47,211,123,246,212,192,
            253,166,109>>,
          <<8,137,155,220,85,62,199,145,9,38,65,199,242,219,66,156,
            206,242,177,63,20,157,142,209,189,127,56,74,70,74,212,
            15>>},
      dev7 =>
          {<<176,10,209,167,143,154,158,191,206,16,219,236,97,24,
            236,72,148,34,129,234,43,229,223,238,10,135,161,54,50,
            36,115,88>>,
          <<5,118,180,252,16,250,71,123,185,141,27,77,23,169,163,
            10,54,156,109,113,0,179,83,97,165,28,12,204,48,251,15,
            15>>},
      dev8 =>
          {<<136,20,44,16,131,198,42,103,160,109,108,194,204,201,
            103,22,238,234,61,182,84,40,10,63,89,119,6,50,48,241,
            51,68>>,
          <<189,86,222,10,200,8,125,16,75,228,16,2,69,88,86,124,
            196,196,74,231,204,30,140,220,50,0,213,232,87,225,179,
            103>>},
      dev9 =>
          {<<0,235,34,204,223,10,215,127,234,5,208,175,1,85,7,55,16,
            64,67,18,77,194,25,162,241,245,46,117,43,78,64,93>>,
          <<166,3,240,163,23,187,156,247,30,198,162,200,114,2,8,
            48,251,47,7,114,197,245,241,92,102,9,13,130,104,135,
            227,84>>},
      parent_chain_dev1 =>
          {<<152,157,72,75,90,78,119,172,146,140,132,149,215,62,100,
            48,215,197,120,100,82,83,160,103,163,73,29,7,171,210,
            38,94>>,
          <<129,79,62,177,181,206,2,8,15,136,7,112,217,165,59,109,
            67,174,209,152,243,78,26,161,165,144,17,136,154,160,37,
            74>>},
      parent_chain_dev2 =>
          {<<224,240,1,149,193,216,134,242,20,94,121,2,43,128,35,
            174,84,234,133,13,68,71,17,223,162,149,120,15,197,179,
            80,102>>,
          <<230,90,41,75,28,169,248,131,25,110,214,187,164,24,123,
            92,159,214,148,83,126,221,210,32,169,118,128,238,57,6,
            185,109>>},
      parent_chain_dev3 =>
          {<<184,22,185,22,224,0,76,48,40,11,205,8,95,88,209,182,81,
            160,110,235,182,221,34,10,8,73,47,114,210,240,170,118>>,
          <<240,188,36,0,17,14,149,87,100,151,13,98,28,178,137,242,
            25,155,104,219,88,174,144,124,73,224,252,215,89,197,72,
            42>>},
      parent_chain_dev4 =>
          {<<104,28,173,107,195,129,46,167,125,127,130,205,106,206,
            106,147,72,79,255,134,152,202,52,136,244,24,215,195,70,
            188,44,106>>,
          <<152,44,215,212,77,82,253,240,73,209,197,231,238,2,173,
            94,243,110,30,54,119,247,112,233,234,145,55,183,235,93,
            112,101>>},
      parent_chain_dev5 =>
          {<<56,53,222,52,197,26,85,15,47,94,236,60,123,77,254,245,
            107,124,199,42,74,75,224,168,234,201,209,75,218,209,
            121,67>>,
          <<142,113,169,121,192,1,137,191,18,138,12,118,121,52,29,
            37,157,240,91,26,122,124,178,154,35,17,7,235,53,217,
            228,116>>},
      parent_chain_dev6 =>
          {<<248,46,89,15,63,65,54,53,70,107,208,193,8,245,246,213,
            47,176,162,182,160,18,88,169,3,188,64,103,124,167,251,
            90>>,
          <<47,129,232,80,100,61,79,81,171,106,122,116,222,228,112,
            162,107,204,12,21,152,233,132,246,236,23,179,151,118,
            109,42,33>>},
      parent_chain_dev7 =>
          {<<144,223,78,180,161,8,115,80,28,42,138,161,176,230,4,18,
            176,203,193,57,182,121,72,154,167,189,13,204,47,122,18,
            84>>,
          <<157,53,149,47,36,122,243,124,152,119,213,174,248,89,41,
            175,241,69,184,236,134,82,94,243,153,188,50,192,25,104,
            79,70>>},
      parent_chain_dev8 =>
          {<<216,61,96,169,60,229,45,186,114,15,1,135,22,116,7,208,
            108,89,97,86,221,217,9,167,60,80,32,241,121,104,183,124>>,
          <<230,169,101,62,78,89,239,87,133,151,253,44,145,185,172,
            178,157,145,136,166,150,50,137,224,243,123,44,121,102,
            76,161,63>>},
      parent_chain_dev9 =>
          {<<184,206,106,212,15,197,38,23,37,222,155,165,244,142,
            245,125,249,214,26,149,203,101,73,109,129,85,34,243,2,
            211,133,107>>,
          <<193,163,178,189,238,148,179,82,198,151,210,207,116,22,
            9,133,180,79,217,130,248,165,100,99,32,116,59,81,35,
            253,93,97>>}
    }.

peer_keys(Node) ->
    maps:get(Node, static_peer_keys()).

%% Keys for signing / verification
%% the list is generated
static_sign_keys() ->
    #{dev1 =>
          {<<238,121,108,68,47,65,15,139,26,172,250,135,122,63,231,
            52,188,121,206,144,200,39,37,112,172,29,216,205,172,56,
            241,4,217,202,108,173,192,99,13,10,129,124,71,86,232,
            121,148,177,243,254,160,88,174,204,22,114,15,42,51,71,
            75,19,135,16>>,
          <<217,202,108,173,192,99,13,10,129,124,71,86,232,121,148,
            177,243,254,160,88,174,204,22,114,15,42,51,71,75,19,
            135,16>>},
      dev2 =>
          {<<133,191,59,166,119,215,123,78,192,54,29,91,247,72,123,
            72,245,85,161,97,70,225,58,34,166,141,6,63,193,79,58,
            65,40,25,191,50,209,111,19,239,98,126,125,211,15,133,
            93,12,13,125,167,137,94,138,27,55,23,50,106,33,28,222,
            180,102>>,
          <<40,25,191,50,209,111,19,239,98,126,125,211,15,133,93,
            12,13,125,167,137,94,138,27,55,23,50,106,33,28,222,180,
            102>>},
      dev3 =>
          {<<238,230,20,172,221,171,100,208,126,164,204,120,180,48,
            69,184,235,69,115,91,190,182,78,22,50,182,78,251,154,
            80,216,250,207,253,207,144,121,89,70,193,75,247,195,
            248,104,132,11,199,133,103,156,209,167,244,82,126,86,
            51,156,36,165,214,45,50>>,
          <<207,253,207,144,121,89,70,193,75,247,195,248,104,132,
            11,199,133,103,156,209,167,244,82,126,86,51,156,36,165,
            214,45,50>>},
      dev4 =>
          {<<19,209,33,235,243,99,249,17,244,83,157,208,67,83,33,
            139,119,20,229,239,237,246,206,184,133,174,104,102,96,
            5,207,182,192,31,158,151,28,63,71,26,231,38,221,211,1,
            236,192,249,253,133,4,7,40,158,64,41,152,22,243,153,70,
            36,70,205>>,
          <<192,31,158,151,28,63,71,26,231,38,221,211,1,236,192,
            249,253,133,4,7,40,158,64,41,152,22,243,153,70,36,70,
            205>>},
      dev5 =>
          {<<130,57,0,252,125,223,103,33,213,53,33,3,148,254,62,248,
            13,252,73,243,15,106,186,244,19,144,203,145,206,73,96,
            222,94,24,229,18,45,34,48,170,24,33,130,139,222,202,
            138,31,109,203,27,234,75,242,54,223,22,223,116,214,76,
            51,189,227>>,
          <<94,24,229,18,45,34,48,170,24,33,130,139,222,202,138,31,
            109,203,27,234,75,242,54,223,22,223,116,214,76,51,189,
            227>>},
      dev6 =>
          {<<213,210,216,82,45,73,194,110,55,42,70,119,117,218,208,
            86,36,30,165,33,37,6,148,78,40,155,166,111,7,178,195,
            78,201,235,163,147,251,30,17,140,65,38,146,165,247,148,
            2,65,220,207,17,100,204,179,249,55,94,32,144,206,37,
            214,113,74>>,
          <<201,235,163,147,251,30,17,140,65,38,146,165,247,148,2,
            65,220,207,17,100,204,179,249,55,94,32,144,206,37,214,
            113,74>>},
      dev7 =>
          {<<23,225,229,78,83,213,119,132,130,125,119,28,227,127,
            178,70,160,29,134,81,196,237,142,41,162,246,9,96,226,
            23,167,180,240,82,159,206,78,38,116,71,179,66,16,138,
            113,187,72,88,139,238,16,181,101,251,194,72,47,157,222,
            79,22,211,112,227>>,
          <<240,82,159,206,78,38,116,71,179,66,16,138,113,187,72,
            88,139,238,16,181,101,251,194,72,47,157,222,79,22,211,
            112,227>>},
      dev8 =>
          {<<93,151,183,72,36,64,241,63,126,143,133,41,156,216,134,
            148,90,221,171,205,222,196,81,25,223,243,180,129,27,21,
            89,79,77,60,254,236,33,138,239,94,254,193,97,198,81,99,
            190,162,4,189,173,68,119,18,0,222,92,111,253,241,226,
            80,53,4>>,
          <<77,60,254,236,33,138,239,94,254,193,97,198,81,99,190,
            162,4,189,173,68,119,18,0,222,92,111,253,241,226,80,53,
            4>>},
      dev9 =>
          {<<58,69,166,171,245,55,98,209,199,199,55,207,76,28,41,73,
            119,46,172,205,2,104,213,157,3,184,23,189,94,15,114,28,
            226,62,38,166,65,203,241,150,173,181,110,139,225,197,
            33,75,180,216,98,49,133,20,146,190,127,66,28,6,212,74,
            252,108>>,
          <<226,62,38,166,65,203,241,150,173,181,110,139,225,197,
            33,75,180,216,98,49,133,20,146,190,127,66,28,6,212,74,
            252,108>>},
      parent_chain_dev1 =>
          {<<187,124,220,143,164,168,142,69,36,116,181,27,69,83,92,
            47,78,12,115,237,223,130,5,199,110,44,175,167,189,15,
            180,147,206,99,118,74,35,102,240,229,173,173,221,213,
            26,19,44,163,29,124,215,157,215,11,246,244,29,162,150,
            78,149,197,26,26>>,
          <<206,99,118,74,35,102,240,229,173,173,221,213,26,19,44,
            163,29,124,215,157,215,11,246,244,29,162,150,78,149,
            197,26,26>>},
      parent_chain_dev2 =>
          {<<91,166,165,234,99,246,184,44,151,233,176,44,86,96,125,
            194,129,222,68,149,92,86,34,72,5,184,120,239,191,167,
            188,125,116,149,29,126,189,154,153,171,39,177,34,120,
            37,114,87,231,224,155,218,229,244,83,241,43,97,237,152,
            181,224,129,144,46>>,
          <<116,149,29,126,189,154,153,171,39,177,34,120,37,114,87,
            231,224,155,218,229,244,83,241,43,97,237,152,181,224,
            129,144,46>>},
      parent_chain_dev3 =>
          {<<210,191,78,110,249,148,32,98,62,209,48,162,188,173,59,
            68,138,26,213,56,188,114,73,83,79,183,203,221,41,38,57,
            49,77,203,228,252,24,157,55,231,112,98,29,127,230,37,
            180,185,145,191,138,212,90,123,66,39,193,243,83,0,99,
            254,31,149>>,
          <<77,203,228,252,24,157,55,231,112,98,29,127,230,37,180,
            185,145,191,138,212,90,123,66,39,193,243,83,0,99,254,
            31,149>>},
      parent_chain_dev4 =>
          {<<60,27,95,88,121,72,212,243,78,84,19,39,143,113,79,1,
            194,204,211,84,220,56,180,177,113,46,189,55,49,43,254,
            69,254,162,12,48,239,161,189,154,139,86,241,76,31,61,
            72,43,166,22,13,127,18,215,147,113,188,68,98,181,65,
            216,209,153>>,
          <<254,162,12,48,239,161,189,154,139,86,241,76,31,61,72,
            43,166,22,13,127,18,215,147,113,188,68,98,181,65,216,
            209,153>>},
      parent_chain_dev5 =>
          {<<115,52,181,26,160,160,162,32,71,160,27,100,55,109,84,
            190,99,238,208,142,3,132,208,9,6,176,6,20,30,12,86,60,
            120,167,231,140,99,196,238,225,166,242,135,8,112,193,
            244,57,5,22,90,12,255,238,204,238,245,24,59,67,13,125,
            94,148>>,
          <<120,167,231,140,99,196,238,225,166,242,135,8,112,193,
            244,57,5,22,90,12,255,238,204,238,245,24,59,67,13,125,
            94,148>>},
      parent_chain_dev6 =>
          {<<77,214,50,86,109,92,248,156,133,58,72,164,115,219,171,
            131,72,189,109,91,3,60,20,24,142,127,12,181,135,157,88,
            129,27,0,248,109,9,251,50,81,34,65,231,195,93,90,180,
            15,132,124,74,52,92,249,47,119,164,143,114,164,132,107,
            93,244>>,
          <<27,0,248,109,9,251,50,81,34,65,231,195,93,90,180,15,
            132,124,74,52,92,249,47,119,164,143,114,164,132,107,93,
            244>>},
      parent_chain_dev7 =>
          {<<55,247,153,183,13,86,216,83,38,184,106,168,100,155,172,
            91,157,142,6,226,10,86,107,255,214,252,110,110,91,87,
            181,21,108,63,75,221,224,4,241,116,161,68,204,199,114,
            23,54,97,231,190,79,16,97,199,245,174,49,14,198,23,3,
            223,99,17>>,
          <<108,63,75,221,224,4,241,116,161,68,204,199,114,23,54,
            97,231,190,79,16,97,199,245,174,49,14,198,23,3,223,99,
            17>>},
      parent_chain_dev8 =>
          {<<251,133,142,175,51,47,152,65,95,125,165,80,218,72,163,
            252,88,114,2,239,124,167,219,69,78,162,64,37,235,154,
            208,100,157,216,130,182,189,103,229,92,222,153,235,167,
            36,14,33,30,109,151,8,60,202,17,145,26,238,50,243,80,
            123,208,66,18>>,
          <<157,216,130,182,189,103,229,92,222,153,235,167,36,14,
            33,30,109,151,8,60,202,17,145,26,238,50,243,80,123,208,
            66,18>>},
      parent_chain_dev9 =>
          {<<69,248,225,159,83,57,226,53,120,29,185,63,24,72,25,231,
            68,104,19,104,171,163,180,60,199,238,46,77,109,142,64,
            84,166,117,56,234,83,194,148,211,232,184,236,33,221,
            202,170,244,135,62,219,175,48,34,23,29,150,67,40,135,
            227,17,169,202>>,
          <<166,117,56,234,83,194,148,211,232,184,236,33,221,202,
            170,244,135,62,219,175,48,34,23,29,150,67,40,135,227,
            17,169,202>>}
     }.

sign_keys(Node) ->
    maps:get(Node, static_sign_keys()).

patron() ->
    aecore_env:patron_keypair_for_testing().

-spec known_mocks() -> #{atom() => #mock_def{}}.
known_mocks() -> #{}.

%%%=============================================================================
%%% API
%%%=============================================================================

init_per_suite(NodesList, CTConfig) ->
    init_per_suite(NodesList, #{}, CTConfig).

init_per_suite(NodesList, CustomNodeConfig, CTConfig) ->
    init_per_suite(NodesList, CustomNodeConfig, [], CTConfig).

init_per_suite(NodesList, CustomNodeCfg, NodeCfgOpts, CTConfig) ->
    DataDir = ?config(data_dir, CTConfig),
    TopDir = top_dir(DataDir),
    CTConfig1 = [{top_dir, TopDir} | CTConfig],
    make_shortcut(CTConfig1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    create_configs(NodesList, CTConfig1, CustomNodeCfg, NodeCfgOpts),
    make_multi(CTConfig1, NodesList),
    CTConfig1.

node_config(Node, CTConfig) ->
    NodeConfig = node_config_dir(Node, CTConfig),
    [OrigCfg] = jsx:consult(NodeConfig, [return_maps]),
    backup_config(NodeConfig),
    OrigCfg.

create_configs(NodesList, CTConfig, CustomConfig, Options) ->
    [create_config(N, CTConfig, CustomConfig, Options) || N <- NodesList].

create_config(Node, CTConfig, CustomConfig, Options) ->
    DbBackendConfig = case os:getenv("AETERNITY_TESTCONFIG_DB_BACKEND") of
        false ->
            ct:fail("The mandatory environment variable is not set: AETERNITY_TESTCONFIG_DB_BACKEND");
        Backend ->
            #{<<"chain">> => #{<<"db_backend">> => binary:list_to_bin(Backend)}}
    end,
    FinalConfig =
        case ?config(use_config_defaults, CTConfig) of
            false ->
                CustomConfig;
            _ -> %% true or undefined
                Forks =
                    case CustomConfig of
                        #{ <<"chain">> := #{<<"hard_forks">> := _ }} -> #{}; %% already set
                        _ -> #{ <<"chain">> => #{<<"hard_forks">> => forks()}}
                    end,
                MergedCfg = maps_merge(default_config(Node, CTConfig), CustomConfig),
                MergedCfg1 = aec_metrics_test_utils:maybe_set_statsd_port_in_user_config(Node, MergedCfg, CTConfig),
                MergedCfg2 = maps_merge(MergedCfg1, DbBackendConfig),
                MergedCfg3 = case proplists:get_value(instant_mining, CTConfig) of
                                undefined ->
                                    ct:log("Instant mining consensus disabled in node"),
                                    MergedCfg2;
                                _ ->
                                    ct:log("Instant mining consensus enabled in node"),
                                    maps_merge(MergedCfg2,
                                        #{<<"chain">> =>
                                            #{<<"consensus">> =>
                                                #{
                                                    <<"0">> =>
                                                        #{
                                                            <<"type">> => <<"ct_tests">>
                                                        }
                                                }
                                            }
                                        })
                            end,
                MergedCfg4 = maps_merge(MergedCfg3, Forks),
                Ports =
                    #{ <<"sync">> => #{ <<"port">> => sync_port(Node)},
                    <<"http">> => #{ <<"external">> => #{<<"port">> => external_api_port(Node)},
                                        <<"internal">> => #{<<"port">> => internal_api_port(Node)},
                                        <<"rosetta">> => #{<<"port">> => rosetta_api_port(Node)},
                                        <<"rosetta_offline">> => #{<<"port">> => rosetta_offline_api_port(Node)}},
                    <<"websocket">> => #{<<"channel">> => #{<<"port">> => ws_port(Node)}}},
                MergedCfg5 = maps_merge(MergedCfg4, Ports),
                FConfig = config_apply_options(Node, MergedCfg5, Options),
                write_keys(Node, FConfig),
                FConfig
        end,
    NodeCfgPath = node_config_dir(Node, CTConfig),
    ok = filelib:ensure_dir(NodeCfgPath),
    write_config(NodeCfgPath, FinalConfig).

create_seed_file(FileNames, Data) when
    is_list(FileNames) ->
    [create_seed_file(FileName, Data) ||
        FileName <- FileNames];
create_seed_file(FileName, Data) ->
    ok = filelib:ensure_dir(FileName),
    write_config(FileName, Data, false).

hard_fork_filename(Node, CTConfig, Consensus, FileName) ->
    Path = filename:join([data_dir(Node, CTConfig), "aecore", "." ++ Consensus, FileName]),
    {ok, list_to_binary(Path)}.

maps_merge(V1, V2) when not is_map(V1); not is_map(V2) ->
    V2;
maps_merge(Map1, Map2) ->
    lists:foldl(fun({K, V}, Map) ->
                    case maps:is_key(K, Map) of
                        false -> Map#{K => V};
                        true  -> Map#{K => maps_merge(V, maps:get(K, Map))}
                    end
                end, Map2, maps:to_list(Map1)).

make_multi(Config, NodesList) ->
    make_multi(Config, NodesList, "test").

make_multi(Config, NodesList, RefRebarProfile) ->
    ct:log("RefRebarProfile = ~p", [RefRebarProfile]),
    Top = proplists:get_value(top_dir, Config),
    ct:log("Top = ~p", [Top]),
    Root = filename:join(Top, "_build/" ++ RefRebarProfile ++ "/rel/aeternity"),
    [setup_node(N, Top, Root, Config) || N <- NodesList].

make_shortcut(Config) ->
    PrivDir  = priv_dir(Config),
    ok = filelib:ensure_dir(filename:join(PrivDir, "foo")),
    Shortcut = shortcut_dir(Config),
    delete_file(Shortcut),
    RelPrivDir = rel_path(PrivDir, filename:dirname(Shortcut)),
    ok = symlink(RelPrivDir, Shortcut).

rel_path(Path, Root) ->
    rel_path(filename:split(Path), filename:split(Root), Path).

rel_path([Dir | Ps], [Dir | Rs], Orig) ->
    rel_path(Ps, Rs, Orig);
rel_path(Ps, [], _) ->
    filename:join(Ps);
rel_path(_, _, Orig) ->
    Orig.

start_node(N, Config) ->
    start_node(N, Config, []).

start_node(N, Config, ExtraEnv) ->
    MyDir = filename:dirname(code:which(?MODULE)),
    ConfigFilename = proplists:get_value(config_name, Config, "default"),
    Flags =
        case ?config(build_to_connect_to_mainnet, Config) of
            true -> %% no proxy!
                ["-pa ", MyDir];
            _ ->
                ["-pa ", MyDir, " -config ./" ++ ConfigFilename]
        end,
    Env0 =
        case ?config(build_to_connect_to_mainnet, Config) of
            true ->
                [
                    {"ERL_FLAGS", Flags},
                    {"AETERNITY_CONFIG", "data/aeternity.json"},
                    {"RUNNER_LOG_DIR","log"}
                ];
            _ ->
                [
                    {"ERL_FLAGS", Flags},
                    {"AETERNITY_CONFIG", "data/aeternity.json"},
                    {"RUNNER_LOG_DIR","log"},
                    {"CODE_LOADING_MODE", "interactive"}
                ]
        end,
    Env = maybe_override(ExtraEnv, Env0),
    cmd(?OPS_BIN, node_shortcut(N, Config), "bin", ["daemon"], Env).

maybe_override([{K,_} = H|T], L0) ->
    [H | maybe_override(T, lists:keydelete(K, 1, L0))];
maybe_override([], L0) ->
    L0.

stop_node(N, Config) ->
    ct:log("Stopping node ~p", [N]),
    case cmd(?OPS_BIN, node_shortcut(N, Config), "bin", ["stop"]) of
        {ok, 0, _Res} ->
            ct:log("Node ~p stopped", [N]),
            ok;
        Else ->
            ct:log("Failed stopping node ~p, Reason: ~p", [N, Else]),
            Else
    end.

reinit_with_bitcoin_ng(N) ->
    ct:log("Reinitializing chain on ~p with bitcoin ng consensus", [N]),
    Node = node_name(N),
    ok = set_env(Node, aecore, consensus, #{<<"0">> => #{<<"type">> => <<"pow_cuckoo">>}}),
    ok = rpc:call(Node, aec_conductor, reinit_chain, []).

reinit_nodes_with_ct_consensus(Nodes) ->
    ct:log("Reinitializing chain on ~p with ct_tests consensus", [Nodes]),
    NodeNames = [node_name(N) || N <- Nodes],
    Timeout = 5000,
    [{ok, maintenance} = rpc:call(NN, app_ctrl, set_and_await_mode, [maintenance, Timeout])
     || NN <- NodeNames],
    [ok = set_env(NN, aecore, consensus, #{<<"0">> => #{<<"type">> => <<"ct_tests">>}})
     || NN <- NodeNames],
    [ok = rpc:call(NN, aec_conductor, reinit_chain, []) || NN <- NodeNames],
    [{ok, normal} = rpc:call(NN, app_ctrl, set_and_await_mode, [normal, Timeout])
     || NN <- NodeNames],
    ok.

reinit_with_ct_consensus(N) ->
    reinit_nodes_with_ct_consensus([N]).

get_node_db_config(Rpc) when is_function(Rpc, 3) ->
    IsDbPersisted = Rpc(application, get_env, [aecore, persist, false]),
    MaybeMnesiaDir =
        case Rpc(application, get_env, [mnesia, dir]) of
            undefined -> undefined;
            {ok, MnesiaDir0} ->
                {ok, Rpc(filename, absname, [MnesiaDir0])}
        end,
    ct:log("Is DB persisted? ~p. What is Mnesia dir if any? ~p",
           [IsDbPersisted, MaybeMnesiaDir]),
    {ok, {IsDbPersisted, MaybeMnesiaDir}}.

delete_node_db_if_persisted({false, undefined}) ->
    ok;
delete_node_db_if_persisted({true, {ok, MnesiaDir}}) ->
    ct:log("Deleting Mnesia Dir ~p", [MnesiaDir]),
    {true, _} = {filelib:is_file(MnesiaDir), MnesiaDir},
    {true, _} = {filelib:is_dir(MnesiaDir), MnesiaDir},
    cmd("rm", ".", ".", ["-r", MnesiaDir], [], false),
    {false, _} = {filelib:is_file(MnesiaDir), MnesiaDir},
    ok.

rpc_test_consensus_enabled(Node) ->
    ConsensusModule = rpc:call(Node, aec_conductor, get_active_consensus_module, []),
    ConsensusModule =:= aec_consensus_common_tests.


rpc_on_demand_consensus_enabled(Node) ->
  aec_consensus_on_demand =:= rpc:call(Node, aec_conductor, get_active_consensus_module, []).

rpc_consensus_request(Node, Request) ->
    rpc:call(Node, aec_conductor, consensus_request, [Request]).

expected_mine_rate() ->
    ?DEFAULT_CUSTOM_EXPECTED_MINE_RATE.

mine_key_blocks(Node, NumBlocksToMine) ->
    mine_key_blocks(Node, NumBlocksToMine, #{}).

mine_key_blocks(Node, NumBlocksToMine, Opts) ->
    mine_blocks(Node, NumBlocksToMine, ?DEFAULT_CUSTOM_EXPECTED_MINE_RATE, key, Opts).

mine_micro_blocks(Node, NumBlocksToMine) ->
    mine_micro_blocks(Node, NumBlocksToMine, #{}).

mine_micro_blocks(Node, NumBlocksToMine, Opts) ->
    mine_blocks(Node, NumBlocksToMine, ?DEFAULT_CUSTOM_EXPECTED_MINE_RATE, micro, Opts).

mine_blocks(Node, NumBlocksToMine) ->
    mine_blocks(Node, NumBlocksToMine, #{}).

mine_blocks(Node, NumBlocksToMine, Opts) when is_map(Opts) ->
    mine_blocks(Node, NumBlocksToMine, ?DEFAULT_CUSTOM_EXPECTED_MINE_RATE, any, Opts).

mine_blocks(Node, NumBlocksToMine, MiningRate, Opts) ->
    mine_blocks(Node, NumBlocksToMine, MiningRate, any, Opts).

hc_mine_blocks(Nodes, NumBlocksToMine) ->
    hc_mine_blocks(Nodes, NumBlocksToMine, 5000, #{}).

mine_blocks(Node, NumBlocksToMine, MiningRate, Type, Opts) ->
    case rpc_test_consensus_enabled(Node)
        orelse rpc_on_demand_consensus_enabled(Node) of
        true ->
            rpc_consensus_request(Node, {mine_blocks, NumBlocksToMine, Type});
        false ->
            Fun = fun() ->
                mine_blocks_loop(NumBlocksToMine, Type)
            end,
            mine_safe_setup(Node, MiningRate, Opts, Fun)
    end.

mine_all_txs(Node, MaxBlocks) ->
    case rpc:call(Node, aec_tx_pool, peek, [infinity]) of
        {ok, []} -> {ok, []};
        {ok, Txs} ->
            TxsHs = [ aeser_api_encoder:encode(tx_hash, aetx_sign:hash(T)) || T <- Txs],
            mine_blocks_until_txs_on_chain(Node, TxsHs, MaxBlocks)
    end.

wait_for_tx_in_pool(Node, Tx) ->
    wait_for_tx_in_pool(Node, Tx, 5000).

wait_for_tx_in_pool(Node, SignedTx, Timeout) ->
    ok = subscribe(Node, tx_created),
    ok = subscribe(Node, tx_received),
    Hash = aetx_sign:hash(SignedTx),
    case rpc:call(Node, aec_chain, find_tx_location, [Hash]) of
        mempool -> ok;
        B when is_binary(B) -> error({already_on_chain, SignedTx});
        none -> error({already_gc, SignedTx});
        not_found ->
            receive
                {gproc_ps_event, tx_received, #{info := SignedTx}} -> ok;
                {gproc_ps_event, tx_created, #{info :=SignedTx}} -> ok
            after
                Timeout ->
                    ct:log("timeout waiting for transaction ~p~n"
                            "~p", [SignedTx, process_info(self(), messages)]),
                    error({timeout_waiting_for_tx, SignedTx})
            end,
	    ok = unsubscribe(Node, tx_created),
	    ok = unsubscribe(Node, tx_received)
    end.

mine_blocks_until_txs_on_chain(Node, TxHashes, MaxBlocks) ->
    mine_blocks_until_txs_on_chain(Node, TxHashes, ?DEFAULT_CUSTOM_EXPECTED_MINE_RATE, MaxBlocks).

mine_blocks_until_txs_on_chain(Node, TxHashes, MiningRate, Max) ->
    mine_blocks_until_txs_on_chain(Node, TxHashes, MiningRate, Max, #{}).

mine_blocks_until_txs_on_chain(Node, TxHashes, MiningRate, Max, Opts) ->
    %% Fail early rather than having to wait until max_reached if txs already on-chain
    ok = assert_not_already_on_chain(Node, TxHashes),
    case rpc_test_consensus_enabled(Node)
        orelse rpc_on_demand_consensus_enabled(Node) of
        true ->
            rpc_consensus_request(Node, {mine_blocks_until_txs_on_chain, TxHashes, Max});
        false ->
            Fun = fun() ->
                mine_blocks_until_txs_on_chain_loop(Node, TxHashes, Max, [])
            end,
            mine_safe_setup(Node, MiningRate, Opts, Fun)
    end.

mine_micro_block_emptying_mempool_or_fail(Node) ->
    case rpc_test_consensus_enabled(Node)
        orelse rpc_on_demand_consensus_enabled(Node) of
        true ->
            rpc_consensus_request(Node, mine_micro_block_emptying_mempool_or_fail);
        false ->
            Fun = fun() ->
                      mine_blocks_loop(2, any)
                  end,
            {ok, Blocks} = mine_safe_setup(Node, expected_mine_rate(), #{}, Fun),
            [key, micro] = [aec_blocks:type(B) || B <- Blocks],

            case rpc:call(Node, aec_tx_pool, peek, [infinity]) of
                {ok, []} ->
                    {ok, Blocks};
                {ok, [_|_]} ->
                    %% So, Tx(s) is/are back in the mempool, this means (unless some Txs arrived
                    %% from thin air) that we had a micro-fork. Let's check what state we stopped in
                    {ok, NewBlocks} = mine_safe_setup(Node, expected_mine_rate(), #{}, Fun),
                    [key, micro] = [aec_blocks:type(B) || B <- NewBlocks],
                    {ok, NewBlocks}
            end
    end.

assert_not_already_on_chain(Node, TxHashes) ->
    Lookup = lists:map(
               fun(TxHash) ->
                       {TxHash, rpc:call(Node, aec_chain, find_tx_location, [TxHash])}
               end, TxHashes),
    case [T || {T, W} <- Lookup, is_binary(W)] of
        [] ->
            ok;
        [_|_] = AlreadyOnChain ->
            error({already_on_chain, AlreadyOnChain})
    end.

mine_blocks_until_txs_on_chain_loop(_Node, _TxHashes, 0, _Acc) ->
    {error, max_reached};
mine_blocks_until_txs_on_chain_loop(Node, TxHashes, Max, Acc) ->
    case mine_blocks_loop(1, key) of
        {ok, [Block]} -> %% We are only observing key blocks
            NewAcc = [Block | Acc],
            case txs_not_on_chain(Node, Block, TxHashes) of
                []        -> {ok, NewAcc};
                TxHashes1 -> mine_blocks_until_txs_on_chain_loop(Node, TxHashes1, Max - 1, NewAcc)
            end;
        {error, _} = Error -> Error
    end.

txs_not_on_chain(Node, Block, TxHashes) ->
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    case rpc:call(Node, aec_chain, get_generation_by_hash, [BlockHash, backward]) of
        error -> TxHashes;
        {ok, #{micro_blocks := MBs }} -> txs_not_in_generation(MBs, TxHashes)
    end.

txs_not_in_generation([], TxHashes) -> TxHashes;
txs_not_in_generation([MB | MBs], TxHashes) ->
    txs_not_in_generation(MBs, txs_not_in_microblock(MB, TxHashes)).

txs_not_in_microblock(MB, TxHashes) ->
    [ TxHash || TxHash <- TxHashes, not tx_in_microblock(MB, TxHash) ].

tx_in_microblock(MB, TxHash) ->
    lists:any(fun(STx) ->
                      aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx)) == TxHash
              end, aec_blocks:txs(MB)).

hc_loop_mine_blocks(NBlocks, Timeout) ->
    hc_loop_mine_blocks_([], NBlocks, Timeout).

hc_loop_mine_blocks_(Blocks, 0, _Timeout) ->
    {ok, Blocks};
hc_loop_mine_blocks_(Blocks, N, Timeout) ->
    case wait_for_new_block_mined(Timeout) of
        {ok, Node, Block} ->
            N1 = case aec_blocks:type(Block) of
                     micro -> N;
                     key   ->
                        case aec_headers:is_hole(aec_blocks:to_key_header(Block)) of
                            true -> N;
                            false -> N - 1
                        end
                 end,
            hc_loop_mine_blocks_([{Node, Block} | Blocks], N1, Timeout);
        Err = {error, _} ->
            Err
    end.

mine_blocks_loop(Cnt, Type) ->
    mine_blocks_loop([], Cnt, Type).

mine_blocks_loop(Blocks, 0, _Type) ->
    {ok, Blocks};
mine_blocks_loop(Blocks, BlocksToMine, Type) when is_integer(BlocksToMine), BlocksToMine > 0 ->
    {ok, _Node, Block} = wait_for_new_block_mined(),
    case aec_blocks:type(Block) of
        Type1 when Type =/= any andalso Type =/= Type1 ->
            %% Don't decrement
            mine_blocks_loop(Blocks, BlocksToMine, Type);
        _ ->
            mine_blocks_loop([Block | Blocks], BlocksToMine - 1, Type)
    end.

wait_for_new_block_mined() ->
    wait_for_new_block_mined(30000).

td(B) ->
    aeu_time:now_in_msecs() - aec_blocks:time_in_msecs(B).

wait_for_new_block_mined(T) when is_integer(T), T >= 0 ->
    receive
        {gproc_ps_event, block_created, Info} ->
            #{info := Block, test_node := Node} = Info,
            ct:log("~p produced a key-block at ~p (time-diff: ~p)", [Node, aec_blocks:height(Block), td(Block)]),
            {ok, Node, Block};
        {gproc_ps_event, micro_block_created, Info} ->
            #{info := Block, test_node := Node} = Info,
            ct:log("~p produced a micro-block at ~p (time-diff ~p)", [Node, aec_blocks:height(Block), td(Block)]),
            {ok, Node, Block}
    after
        T ->
            case T of
                0 ->
                    not_logging;
                _ ->
                    ct:log("timeout waiting for block event~n"
                           "~p", [process_info(self(), messages)])
            end,
            {error, timeout_waiting_for_block}
    end.

wait_for_new_block(T) when is_integer(T), T >= 0 ->
    receive
        {gproc_ps_event, top_changed, #{info := #{block_hash := _Hash}}} ->
            ok
    after
        T ->
            case T of
                0 ->
                    not_logging;
                _ ->
                    ct:log("timeout waiting for block event~n"
                           "~p", [process_info(self(), messages)])
            end,
            {error, timeout_waiting_for_block}
    end.

flush_new_blocks() ->
    flush_new_blocks_([]).

flush_new_blocks_(Acc) ->
    case wait_for_new_block_mined(0) of
        {error, timeout_waiting_for_block} ->
            lists:reverse(Acc);
        {ok, _Node, Block} ->
            flush_new_blocks_([Block | Acc])
    end.

flush_new_blocks_produced() ->
    case wait_for_new_block(0) of
        {error, timeout_waiting_for_block} -> ok;
        ok ->
            flush_new_blocks_produced()
    end.

%% Try to flush at least N messages from mempool
%% (the N is to avoid races - so it just waits "a little bit").
flush_mempool(N, Node) ->
    flush_mempool(2, N, Node).

flush_mempool(Retries, N, Node) ->
    case rpc:call(Node, aec_tx_pool, peek, [infinity]) of
        {ok, SignedTxs} when length(SignedTxs) >= N orelse Retries == 0 ->
            [ rpc:call(Node, aec_tx_pool, delete, [aetx_sign:hash(STx)]) || STx <- SignedTxs ],
            ct:log("Flushed ~p txs from ~p's mempool", [length(SignedTxs), Node]);
        _ ->
            timer:sleep(500),
            flush_mempool(Retries - 1, N, Node)
    end.

%% block the process until a certain height is reached
%% this has the expectation that the Node is mining
%% there is a timeout of 30 seconds for a single block to be produced
wait_for_height(Node, Height) ->
    wait_for_height(Node, Height, 30000).

wait_for_height(Node, Height, TimeoutPerBlock) ->
    flush_new_blocks_produced(),
    ok = subscribe(Node, top_changed),
    ok = wait_for_height_(Node, Height, TimeoutPerBlock),
    ok = unsubscribe(Node, top_changed),
    ok.

wait_for_height_(Node, Height, TimeoutPerBlock) ->
    TopHeight =
        case rpc:call(Node, aec_chain, top_header, []) of
            undefined -> 0;
            Header -> aec_headers:height(Header)
        end,
    case TopHeight >= Height of
        true -> % reached height
            ok;
        false ->
            case wait_for_new_block(TimeoutPerBlock) of
                {error, timeout_waiting_for_block} ->
                    {error, timeout_waiting_for_block, {top, TopHeight}, {waiting_for, Height}};
                ok ->
                    wait_for_height_(Node, Height, TimeoutPerBlock)
            end
    end.

spend(Node, FromPub, ToPub, Amount, Fee) ->
    {ok, Nonce} = rpc:call(Node, aec_next_nonce, pick_for_account, [FromPub]),
    Params = #{sender_id    => aeser_id:create(account, FromPub),
               recipient_id => aeser_id:create(account, ToPub),
               amount       => Amount,
               fee          => Fee,
               nonce        => Nonce,
               payload      => <<"foo">>},

    {ok, Tx} = rpc:call(Node, aec_spend_tx, new, [Params]),
    {ok, SignedTx} = sign_on_node({dev1, Node}, Tx),
    ok = rpc:call(Node, aec_tx_pool, push, [SignedTx]),
    {ok, SignedTx}.

sign_on_node(Id, Tx) ->
    sign_on_node(Id, Tx, false).

sign_on_node({Id, _Node}, Tx, SignHash) ->
    {SignPrivKey, _} = sign_keys(Id),
    {ok, aec_test_utils:sign_tx(Tx, SignPrivKey, SignHash)};
sign_on_node(Id, Tx, SignHash) ->
    sign_on_node(node_tuple(Id), Tx, SignHash).

protocol_to_heights() ->
    NetworkId =
        case os:getenv("PROTOCOL") of
            false ->
                {ok, NId} = application:get_env(aecore, network_id),
                NId;
            P ->
                Protocol = list_to_binary(P),
                <<"local_", Protocol/binary, "_testnet">>
        end,
    aec_hard_forks:protocols_from_network_id(NetworkId).

forks() ->
  maps:fold(fun(K,V,Acc) -> Acc#{K => V} end, #{}, protocol_to_heights()).

latest_fork_height() ->
    lists:max(maps:values(protocol_to_heights())).

latest_protocol_version() ->
    lists:max(maps:keys(aec_hard_forks:protocols())).

mock_mempool_nonce_offset(Node, Offset) ->
    ok = set_env(Node, aecore, mempool_nonce_offset, Offset).

unmock_mempool_nonce_offset(Node) ->
    ok = unset_env(Node, aecore, mempool_nonce_offset).


top_dir(DataDir) ->
    %% Split the DataDir path at "_build"
    [Top, _] = re:split(DataDir, "_build", [{return, list}]),
    Top.

node_tuple(N) ->
    {N, node_name(N)}.

node_name(N) ->
    [_,H] = re:split(atom_to_list(node()), "@", [{return,list}]),
    list_to_atom("aeternity_" ++ atom_to_list(N) ++ "@" ++ H).

parent_chain_node(Idx) ->
    list_to_atom("parent_chain_" ++ "dev" ++ integer_to_list(Idx)).

-spec connect(atom(), list(atom()) | all_mocks) -> ok.
connect(N, all_mocks) ->
    Mocks = maps:keys(known_mocks()),
    connect(N, Mocks);
connect(N, Mocks) when is_list(Mocks) ->
    ok = connect(N),
    start_mocks(N, Mocks),
    ok.

-spec connect(atom()) -> ok.
connect(N) ->
    connect_wait(N, aehttp).

-spec connect_wait(atom(), atom()) -> ok.
connect_wait(N, WaitForApp) ->
    connect_(N, 50, fun() -> await_app(N, WaitForApp) end),
    report_node_config(N),
    ok.

connect_await_mode(N) ->
    connect_(N, 50, fun() -> rpc:call(N, app_ctrl, await_stable_mode, [30000]) end).

subscribe(N, Event) ->
    call_proxy(N, {subscribe, Event}).

unsubscribe(N, Event) ->
    ok = call_proxy(N, {unsubscribe, Event}),
    flush_events(N, Event).

flush_events(N, Event) ->
    receive
        {app_started, #{test_node := N}} when Event == app_started ->
            flush_events(N, Event);
        {gproc_ps_event, Event, #{sender := From}} when node(From) == N ->
            flush_events(N, Event)
    after 0 ->
            ok
    end.

all_events_since(N, TS) ->
    [{E, try events_since(N, E, TS) catch error:Err -> Err end}
     || E <- [block_created, micro_block_created, chain_sync, app_started]].

events_since(N, EvType, TS) ->
    call_proxy(N, {events, EvType, TS}).

check_for_logs(Nodes, Config) ->
    [] = [{N, F} || N <- Nodes,
                    F <- expected_logs(),
                    is_missing_log(N, F, Config)],
    ok.

is_missing_log(N, F, Config) ->
    LogDir = log_dir(N, Config),
    file_missing(filename:join(LogDir, F)).

file_missing(F) ->
    case file:read_link_info(F) of
        {ok, _} ->
            false;
        _ ->
            true
    end.

assert_no_errors_in_logs(Config) ->
    assert_no_errors_in_logs(Config, []).

%% Scans the logs for '[error]' patterns and fails if any are found.
%% If AllowedSubstrings is provided, then any log lines containing these substrings are not reported.
-spec assert_no_errors_in_logs(Config :: proplists:proplist(), AllowedPatterns :: [string()]) -> ok.
assert_no_errors_in_logs(Config, AllowedSubstrings) ->
    Nodes = [Node || {Node, _, _} <- ?config(nodes, Config)],
    Group = proplists:get_value(name, proplists:get_value(tc_group_properties, Config, []), "?"),
    {IgnoredErrors, ErrorsToReport} = lists:partition(
        fun({_Node, {_File, Line}}) ->
            lists:any(
                fun(Pattern) -> string:find(Line, Pattern) =/= nomatch end,
                AllowedSubstrings
            )
        end,
        errors_in_logs(Nodes, Config)),

    case IgnoredErrors of
        [] -> ok;
        _ -> ct:pal("Ignoring errors found in logs, while running group=~s~n~150p", [Group, IgnoredErrors])
    end,
    case ErrorsToReport of
        [] -> ok;
        _ ->
            ct:pal("Errors found in logs, while running group=~s~n~150p", [Group, ErrorsToReport]),
            ?assert(false, "Found errors in logs. To ignore, add substrings to 2nd arg of aecore_suite_utils:assert_no_errors_in_logs/2")
    end,
    ok.

errors_in_logs(Nodes, Config) ->
    [{N, Errs} || N <- Nodes,
                  Errs <- check_errors_logs(N, Config)].

check_errors_logs(Node, Config) ->
    LogDir = log_dir(Node, Config),
    [{F, Errs} || F <- expected_logs(),
                  Errs <- grep_error(filename:join(LogDir, F))].

grep_error(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    Entries = string:lexemes(Bin, [$\r,$\n]),
    [ Entry || Entry <- Entries,
               string:find(Entry, "[error]") =/= nomatch ].

times_in_epoch_log(Node, Config, Str) ->
    LogFile = filename:join(log_dir(Node, Config), "aeternity.log"),
    ct:log("Reading logfile ~p", [LogFile]),
    {ok, Bin} = file:read_file(LogFile),
    Entries = string:lexemes(Bin, [$\r,$\n]),
    [ Entry || Entry <- Entries,
               string:find(Entry, Str) =/= nomatch ].

expected_logs() ->
    ["aeternity.log", "aeternity_mining.log", "aeternity_sync.log",
     "aeternity_pow_cuckoo.log", "aeternity_metrics.log"].

await_sync_complete(T0, Nodes) ->
    [ok = subscribe(N, chain_sync) || N <- Nodes],
    AllEvents = events_since_on_nodes(chain_sync, T0, Nodes),
    ct:log("AllEvents = ~p", [AllEvents]),
    SyncNodes =
        lists:foldl(
          fun(Msg, Acc) ->
                  check_event(Msg, Acc)
          end, Nodes, AllEvents),
    ct:log("SyncNodes = ~p", [SyncNodes]),
    ct:log("Messages = ~p", [process_info(self(), messages)]),
    Res = collect_sync_events(SyncNodes, 100),
    [ok = unsubscribe(N, chain_sync) || N <- Nodes],
    Res.

await_is_syncing(T0, Nodes) ->
    [ok = subscribe(N, chain_sync) || N <- Nodes],
    AllEvents = events_since_on_nodes(chain_sync, T0, Nodes),
    ct:log("AllEvents = ~p", [AllEvents]),
    {Sofar, RestNodes} =
        lists:foldl(
          fun(#{sender := From, info := {is_syncing, Flag}, time := Te}, {Found, Ns} = Acc) ->
                  Node = node(From),
                  case lists:member(Node, Ns) of
                      true ->
                          {[{Node, Flag, Te}|Found], Ns -- [Node]};
                      false ->
                          Acc
                  end;
             (_, Acc) ->
                  Acc
          end, {[], Nodes}, AllEvents),
    [ok = unsubscribe(N, chain_sync) || {N,_,_} <- Sofar],
    Res = collect_is_syncing(RestNodes, Sofar),
    [ok = unsubscribe(N, chain_sync) || N <- RestNodes],
    %% Ensure same order as in the Nodes list
    [{_,_,_} = lists:keyfind(N, 1, Res) || N <- Nodes].

events_since_on_nodes(Event, T0, Nodes) ->
    lists:flatten(
      [events_since(N, Event, T0) || N <- Nodes]
     ).

collect_sync_events(_, 0) -> error(retry_exhausted);
collect_sync_events([], _) -> done;
collect_sync_events(SyncNodes, N) ->
    receive
        {gproc_ps_event, chain_sync, #{info := {chain_sync_done, _}} = Msg} ->
            ct:log("chain_sync event: ~p", [Msg]),
            SyncNodes1 = check_event(Msg, SyncNodes),
            collect_sync_events(SyncNodes1, N-1)
    after 20000 ->
            ct:log("Timeout in collect_sync_events: ~p~n"
                   "~p", [SyncNodes, process_info(self(), messages)]),
            error(timeout)
    end.

collect_is_syncing(Nodes, Acc) ->
    TRef = erlang:start_timer(10000, self(), is_syncing),
    collect_is_syncing(Nodes, TRef, Acc).

collect_is_syncing([], TRef, Acc) ->
    erlang:cancel_timer(TRef),
    Acc;
collect_is_syncing([N|Ns], TRef, Acc) ->
    receive
        {timeout, TRef, _} ->
            error(timeout);
        {gproc_ps_event, chain_sync, #{ sender := From
                                      , info := {is_syncing, Flag}
                                      , time := Te }} when node(From) == N ->
            collect_is_syncing(Ns, TRef, [{N, Flag, Te}|Acc])
    end.

check_event(#{sender := From, info := Info}, Nodes) ->
    case Info of
        {chain_sync_done, _} ->
            lists:delete(node(From), Nodes);
        _ ->
            Nodes
    end.

restart_jobs_server(N) ->
    JobsP = rpc:call(N, erlang, whereis, [jobs_server]),
    true = rpc:call(N, erlang, exit, [JobsP, kill]),
    await_new_jobs_pid(N, JobsP).

delete_file(F) ->
    case file:delete(F) of
        ok -> ok;
        {error, enoent} -> ok;
	 {error, Err} ->
            case {os:type(), Err} of
                {{win32, _}, eperm} ->
                    win32_delete_junction(F);
		 _ ->
            		ct:fail("Could not delete file ~s: ~p~n", [F, Err])
	    end
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

connect_(N, Timeout, WaitF) when Timeout < 10000, is_function(WaitF, 0) ->
    timer:sleep(Timeout),
    case net_kernel:hidden_connect_node(N) of
        true ->
            ct:log("hidden_connect_node(~p) -> true", [N]),
            WaitF(),
            true;
        false ->
            ct:log("hidden_connect_node(~p) -> false, retrying ...", [N]),
            connect_(N, Timeout * 2, WaitF)
    end;
connect_(N, _, _) ->
    ct:log("exhausted retries (~p)", [N]),
    erlang:error({could_not_connect, N}).

report_node_config(N) ->
    [ct:log("~w env: ~p", [A, rpc:call(N, application, get_all_env, [A], 2000)]) ||
        A <- [aeutil, aecore, aehttp]].

start_mocks(N, Mocks) ->
    ct:log("Enabling mocks: ~p for node ~p", [Mocks, N]),
    lists:foreach(fun(Mock) -> start_mock(N, Mock) end, Mocks).

start_mock(N, Mock) ->
    #mock_def{module = Module, init_fun = InitF} = maps:get(Mock, known_mocks()),
    rpc:call(N, Module, InitF, [], 2000).

end_mocks(N, Mocks) ->
    lists:foreach(fun(Mock) -> end_mock(N, Mock) end, Mocks).

end_mock(N, Mock) ->
    #mock_def{module = Module, finish_fun = FinishF} = maps:get(Mock, known_mocks()),
    rpc:call(N, Module, FinishF, [], 2000).

await_aehttp(N) ->
    await_app(N, aehttp).

await_app(N, App) ->
    ok = subscribe(N, app_started),
    Events = events_since(N, app_started, 0),
    ct:log("`app_started` Events since 0: ~p", [Events]),
    case [true || #{info := A} <- Events, A == App] of
        [] ->
            receive
                {app_started, #{info := App}} ->
                    ct:log("~p started", [App]),
                    ok
            after 30000 ->
                    error(list_to_atom("timeout_waiting_for_" ++ atom_to_list(App)))
            end;
        [_|_] ->
            ct:log("~p already started", [App]),
            ok
    end,
    ok = unsubscribe(N, app_started),
    ok.

setup_node(N, Top, Root, Config) ->
    ct:log("setup_node(~p,Config)", [N]),
    {ok, VerContents} = file:read_file(filename:join(Root, "VERSION")),
    [VerB |_ ] = binary:split(VerContents, [<<"\n">>, <<"\r">>], [global]),
    Version = binary_to_list(VerB),
    DDir = node_shortcut(N, Config),
    filelib:ensure_dir(filename:join(DDir, "foo")),
    ok = cp_dir(filename:join([Root, "releases", Version]),
                filename:join([DDir, "releases", Version])),
    cp_dir(filename:join(Root, "bin"), DDir ++ "/"),
    symlink(filename:join(Root, "lib"), filename:join(DDir, "lib"), true),
    symlink(filename:join(Root, "patches"), filename:join(DDir, "patches"), true),
    %%
    SysCfgD = sys_config_dir(Top, N, Config),
    VMCfgD = vm_config_dir(Top, N, Config),
    RelD = filename:dirname(filename:join([DDir, "releases", Version, "aeternity.rel"])),
    [cp_file(filename:join(VMCfgD, F), filename:join(RelD, F)) || F <- ["vm.args"]],
    [cp_file(filename:join(SysCfgD, F), filename:join(RelD, F)) || F <- ["sys.config"]],
    [cp_file(filename:join(Root, F), filename:join(DDir, F)) || F <- ["VERSION", "REVISION"]],
    delete_file(filename:join(RelD, "vm.args.orig")),
    delete_file(filename:join(RelD, "sys.config.orig")),
    set_node_name(N, filename:join(RelD, "vm.args")),
    TestsDir = filename:dirname(code:which(?MODULE)),
    TestD = filename:join(TestsDir, "data"),
    ConfigFilename = proplists:get_value(config_name, Config, "default") ++ ".config",
    cp_file(filename:join(TestD, ConfigFilename),
            filename:join(DDir , ConfigFilename)),
    aec_test_utils:copy_forks_dir(Root, DDir).

%% Take the node specific config if it exists, otherwise just use dev1
sys_config_dir(Top, N, Config) ->
    case ?config(build_to_connect_to_mainnet, Config) of
        true ->
            filename:join([Top, "config"]);
        _ ->
            Dir = filename:join([Top, "config/", N]),
            case filelib:is_dir(Dir) of
                true ->
                    Dir;
                false ->
                    filename:join([Top, "config/", "dev1"])
            end
    end.

vm_config_dir(Top, N, _Config) ->
    Dir = filename:join([Top, "config/", N]),
    case filelib:is_dir(Dir) of
        true ->
            Dir;
        false ->
            filename:join([Top, "config/", "dev1"])
    end.

cp_dir(From, To) ->
    ToDir = case lists:last(To) of
                $/ ->
                    filename:join(To, filename:basename(From));
                _ ->
                    To
            end,
    ok = filelib:ensure_dir(filename:join(ToDir, "foo")),
    cp_dir(file:list_dir(From), From, ToDir).

cp_dir({ok, Fs}, From, To) ->
    ct:log("cp_dir({ok, ~p}, ~p, ~p)", [Fs, From, To]),
    Res =
        lists:foldl(
            fun(F, Acc) ->
                FullF = filename:join(From, F),
                case filelib:is_dir(FullF) of
                    true ->
                        To1 = filename:join(To, F),
                        cp_dir(FullF, To1),
                        [FullF|Acc];
                    false ->
                        Tgt = filename:join(To, F),
                        ok = filelib:ensure_dir(Tgt),
                        ok = file_copy(FullF, Tgt),
                        ok = match_mode(FullF, Tgt),
                        [FullF|Acc]
                end
            end, [], Fs),
    ct:log("cp_dir(~p, ~p) -> ~p", [From, To, Res]),
    ok;
cp_dir({error, _} = Error, From, To) ->
    ct:log("cp_dir(~p, ~p) -> ~p", [From, To, Error]),
    Error.

file_copy(From, To) ->
    case file:copy(From, To) of
        {ok, _} ->
            ok;
        Other ->
            ct:log("ERROR in file:copy(~p, ~p):~n   ~p", [From, To, Other]),
            error(Other, [From, To])
    end.

match_mode(A, B) ->
    case {file:read_link_info(A), file:read_file_info(B)} of
        {{ok, #file_info{mode = M}}, {ok, FI}} ->
            file:write_file_info(B, FI#file_info{mode = M});
        Other ->
            ct:log("Error matching mode ~p -> ~p: ~p", [A, B, Other]),
            {error, {match_mode, {A, B}, Other}}
    end.

cp_file(From, To) ->
    case file:copy(From, To) of
        {ok, _} ->
            ct:log("Copied ~s to ~s", [From, To]);
        Err ->
            ct:fail("Error copying ~s to ~s: ~p", [From, To, Err])
    end,
    ok.

symlink(From, To) ->
    symlink(From, To, false).
symlink(From, To, CanFail) ->
    case {symlink_or_copy(From, To), CanFail} of
        {ok, _} ->
            ct:log("symlinked ~s to ~s", [From, To]);
        {Err, false} ->
            ct:fail("Error symlinking ~s to ~s: ~p", [From, To, Err]);
        {Err, true} ->
            ct:log("Error symlinking ~s to ~s: ~p", [From, To, Err])
    end,
    ok.

cmd(Cmd, Dir, Args) ->
    cmd(Cmd, Dir, ".", Args, []).
cmd(Cmd, Dir, BinDir, Args) ->
    cmd(Cmd, Dir, BinDir, Args, []).
cmd(Cmd, Dir, BinDir, Args, Env) ->
    cmd(Cmd, Dir, BinDir, Args, Env, true).
cmd(C, Dir, BinDir, Args, Env, FindLocalBin) ->
    Cmd = binary_to_list(iolist_to_binary(C)),
    CmdRes = cmd_run(Cmd, Dir, BinDir, Args, Env, FindLocalBin),
    {Fmt, FmtArgs} =
        case cmd_res(CmdRes) of
            {0, Out} ->
                {"> ~s~n~s", [Cmd, Out]};
            {ErrCode, Out} ->
                {"> ~s~nERR ~p: ~s~n", [Cmd, ErrCode, Out]}
        end,
    ct:log(Fmt, FmtArgs),
    CmdRes.

cmd_run(Cmd, Dir, BinDir, Args, Env, FindLocalBin) ->
    %% On Windows we need to ensure that Erlang is available in the path.
    Env1 = case os:type() of
               {win32, _} ->
                   Path = filename:dirname(os:find_executable("erl")),
                   [{"PATH", Path ++ ";%PATH%"} | Env];
               _ ->
                   Env
           end,
    Opts = [
            {env, Env1},
            exit_status,
            overlapped_io,
            stderr_to_stdout,
            hide, %% will note spawn an extra window on win32 for the executed command
            {args, Args},
            {cd, Dir}
           ],
    ct:log("Running command ~p in ~p with ~p, opts ~p", [Cmd, Dir, Args, Opts]),
    Bin = case FindLocalBin of
              true ->
                  os:find_executable(Cmd, filename:join(Dir, BinDir));
              false ->
                  os:find_executable(Cmd)
          end,
    %% A bug in OTP requires us to copy the start script such that it has the extension .bat
    Bin1 = case {os:type(), filename:extension(Bin)} of
               {{win32, _}, ".cmd"} ->
                   NewBin = filename:rootname(Bin) ++ ".bat",
                   ok = symlink(Bin, NewBin),
                   NewBin;
               _ ->
                   Bin
    end,
    Port = erlang:open_port({spawn_executable, Bin1}, Opts),
    WaitFun = fun(Fun, P, Res) ->
                     receive
                         {P, {exit_status, 0}} ->
                             {ok, 0, Res};
                         {P, {exit_status, Err}} ->
                             {error, Err, Res};
                         {P, {data, Msg}} ->
                             Fun(Fun, P, Res ++ Msg);
                         AMsg ->
                             ct:log("Ignoring unrecognized message received: ~p", [AMsg]),
                             Fun(Fun, P, Res)
                     after 30000 -> {error, timeout, Res}
                     end
             end,
    WaitFun(WaitFun, Port, "").

cmd_res({_, Code, L}) ->
    {Code, L}.

set_env(Node, App, Key, Value) ->
    ok = rpc:call(Node, application, set_env, [App, Key, Value], 5000).

unset_env(Node, App, Key) ->
    ok = rpc:call(Node, application, unset_env, [App, Key], 5000).

config_apply_options(_Node, Cfg, []) ->
    Cfg;
config_apply_options(Node, Cfg, [{block_peers, BlockedPeers}| T]) ->
    Cfg1 = Cfg#{<<"blocked_peers">> => [peer_info(P) || P <- BlockedPeers]},
    config_apply_options(Node, Cfg1, T);
config_apply_options(Node, Cfg, [{trusted_peers, Peers}| T]) ->
    Cfg1 = Cfg#{<<"peers">> => Peers},
    config_apply_options(Node, Cfg1, T);
config_apply_options(Node, Cfg, [{add_peers, true}| T]) ->
    Cfg1 = Cfg#{<<"peers">> =>
              [peer_info(N1) || N1 <- [dev1, dev2, dev3] -- [Node]]},
    config_apply_options(Node, Cfg1, T);
config_apply_options(Node, Cfg, [no_peers| T]) ->
    Cfg1 = maps:remove(<<"peers">>, Cfg),
    config_apply_options(Node, Cfg1, T);
config_apply_options(Node, Cfg, [OptNodesCfg | T]) when is_map(OptNodesCfg) ->
    Cfg1 = case OptNodesCfg of
               #{Node := OptNodeCfg} -> maps_merge(Cfg, OptNodeCfg);
               #{} -> Cfg
           end,
    config_apply_options(Node, Cfg1, T).


write_keys(Node, Config) ->
    #{ <<"keys">> := #{ <<"dir">> := Path, <<"peer_password">> := Pwd } } = Config,
    ok = filelib:ensure_dir(filename:join(Path, "foo")),
    {PeerPrivKey, PeerPubKey} = peer_keys(Node),
    ct:log("Writing peer keys (~p, ~p) to ~p (~p)", [PeerPrivKey, PeerPubKey, Path, filelib:is_dir(Path)]),
    ok = file:write_file(filename:join(Path, "peer_key.pub"), aec_keys:encrypt_key(Pwd, PeerPubKey)),
    ok = file:write_file(filename:join(Path, "peer_key"), aec_keys:encrypt_key(Pwd, PeerPrivKey)),
    ok.

write_config(F, Config) ->
    write_config(F, Config, true).

write_config(F, Config, PerformCheck) ->
    JSON = jsx:prettify(jsx:encode(Config)),
    {ok, Fd} = file:open(F, [write]),
    ct:log("Writing config (~p)~n~s", [F, JSON]),
    try io:fwrite(Fd, "~s~n", [JSON])
    after
        file:close(Fd)
    end,
    case PerformCheck of
        true ->
            VRes = aeu_env:check_config(F),
            ct:log("Config (~p) check: ~p", [F, VRes]),
            {ok,_} = VRes;
        false -> {ok, Config}
    end.

default_config(N, Config) ->
    {A,B,C} = os:timestamp(),
    {_PrivKey, PubKey} = sign_keys(N),
    {ok, NetworkId} = application:get_env(aecore, network_id),
    #{<<"keys">> =>
          #{<<"dir">> => iolist_to_binary(keys_dir(N, Config)),
            <<"peer_password">> => iolist_to_binary(io_lib:format("~w.~w.~w", [A,B,C]))},
      <<"logging">> =>
          #{ <<"hwm">> => 5000
           , <<"level">> => <<"debug">>},
      <<"mining">> =>
          #{<<"autostart">> => false,
            <<"beneficiary">> => aeser_api_encoder:encode(account_pubkey, PubKey),
            <<"beneficiary_reward_delay">> => 2},
      <<"chain">> =>
          #{<<"persist">> => true},
      <<"fork_management">> =>
          #{<<"network_id">> => NetworkId},
      <<"include_default_peers">> => false
     }.

node_config_dir(N, Config) ->
    filename:join(data_dir(N, Config), "aeternity.json").

%% dirs
node_shortcut(N, Config) ->
    filename:join(shortcut_dir(Config), N).

shortcut_dir(Config) ->
    Top = ?config(top_dir, Config),
    SymlinkName = ?config(symlink_name, Config),
    filename:join([Top, "_build/test/logs", SymlinkName]).

priv_dir(Config) ->
    SubDir = atom_to_list(?config(test_module, Config)),
    filename:join(?config(priv_dir, Config), SubDir).

data_dir(N, Config) ->
    filename:join(node_shortcut(N, Config), "data").

log_dir(N, Config) ->
    filename:join(node_shortcut(N, Config), "log").

keys_dir(N, Config) ->
    filename:join(data_dir(N, Config), "keys").

%% Use localhost here, because some systems have both 127.0.0.1 and 127.0.1.1
%% defined, resulting in a conflict during testing
peer_info(N) ->
    list_to_binary(["aenode://", aeser_api_encoder:encode(peer_pubkey, pubkey(N)),
                  "@localhost:", integer_to_list(sync_port(N))]).

split_node_name(Node) when is_atom(Node) ->
    split_node_name(atom_to_binary(Node, utf8));
split_node_name(NodeBin) when is_binary(NodeBin) ->
    {Type, IdxBin} =
        case NodeBin of
            <<"dev", I/binary>> -> {node, I};
            <<"parent_chain_dev", I/binary>> -> {parent_node, I}
        end,
    Idx = binary_to_integer(IdxBin),
    true = Idx < ?MAX_NODES_PER_GROUP,
    {Type, Idx}.

port_group(node) -> 3000;
port_group(parent_node) -> 6000.

sync_port(Node) ->
    {NodeGroup, Idx} = split_node_name(Node),
    port_group(NodeGroup) + Idx * 10 + 5. %% dev1: 3015

external_api_port(Node) ->
    {NodeGroup, Idx} = split_node_name(Node),
    port_group(NodeGroup) + Idx * 10 + 3. %% dev1: 3013

internal_api_port(Node) ->
    {NodeGroup, Idx} = split_node_name(Node),
    port_group(NodeGroup) + Idx * 10 + 103. %% dev1: 3113

rosetta_api_port(Node) ->
    {NodeGroup, Idx} = split_node_name(Node),
    port_group(NodeGroup) + Idx * 10 + 203. %% dev1: 3213

rosetta_offline_api_port(Node) ->
    {NodeGroup, Idx} = split_node_name(Node),
    port_group(NodeGroup) + Idx * 10 + 403. %% dev1: 3413

ws_port(Node) ->
    {NodeGroup, Idx} = split_node_name(Node),
    port_group(NodeGroup) + Idx * 10 + 4. %% dev1: 3014

pubkey(N) ->
    {_, PubKey} = peer_keys(N),
    PubKey.

backup_config(NodeConfig) ->
    Dir = filename:dirname(NodeConfig),
    Ext = filename:extension(NodeConfig),
    Base = filename:basename(NodeConfig, Ext),
    {A,B,C} = os:timestamp(),
    BackupBase = lists:flatten(
                   [Base, "-",
                    integer_to_list(A),
                    "-",
                    integer_to_list(B),
                    "-",
                    integer_to_list(C),
                    Ext]),
    Backup = filename:join(Dir, BackupBase),
    ct:log("Back up ~p to ~p", [NodeConfig, Backup]),
    cp_file(NodeConfig, Backup).


%% ============================================================
%% Proxy process
%% ============================================================

-define(PROXY, aeternity_multi_node_test_proxy).
-define(PROXY_CALL_RETRIES, 50).

proxy() ->
    register(?PROXY, self()),
    process_flag(trap_exit, true),
    {ok, _} = aec_test_app_checker:start_link(self()),
    error_logger:info_msg("starting test suite proxy~n", []),
    proxy_loop([{marker, app_started}], dict:new()).

start_proxy() ->
    io:fwrite("starting proxy...~n", []),
    proc_lib:spawn(?MODULE, proxy, []).

proxy_loop(Subs, Events) ->
    receive
        {From, Ref, debug} ->
            From ! {Ref, #{pid => self(),
                           subs => Subs,
                           events => Events}},
            proxy_loop(Subs, Events);
        {From, Ref, {subscribe, Event}} ->
            case lists:keymember(Event, 2, Subs) of
                true ->
                    From ! {Ref, ok},
                    proxy_loop([{From, Event}|Subs], Events);
                false ->
                    case lists:member(Event, events()) of
                        true ->  % pre-subscribed
                            From ! {Ref, ok},
                            proxy_loop([{From, Event}|Subs], Events);
                        false ->
                            case catch aec_events:subscribe(Event) of
                                true ->
                                    From ! {Ref, ok},
                                    proxy_loop(
                                      [{From, Event}|
                                       ensure_markers([Event], Subs)], Events);
                                Other ->
                                    From ! {Ref, Other},
                                    proxy_loop(Subs, Events)
                            end
                    end
            end;
        {From, Ref, {unsubscribe, Event}} ->
            From ! {Ref, ok},
            proxy_loop([S || S <- Subs,
                             S =/= {From, Event}], Events);
        {From, Ref, {events, E, Since}} ->
            Res = case dict:find(E, Events) of
                      error -> [];
                      {ok, Es} ->
                          %% exclude events before or AT Since
                          lists:dropwhile(
                            fun(#{time := T}) -> T =< Since end, Es)
                  end,
            From ! {Ref, Res},
            proxy_loop(Subs, Events);
        {gproc_ps_event, Event, Info0} ->
            Info = Info0#{test_node => node()}, % for easier debugging
            tell_subscribers(Subs, Event, {gproc_ps_event, Event, Info}),
            proxy_loop(Subs, dict:append(Event, Info, Events));
        {application_started, T, App} ->
            Info = #{time => T, info => App, test_node => node()},
            tell_subscribers(Subs, app_started, {app_started, Info}),
            Subs1 = case App of
                        gproc ->
                            Es = set_subscriptions(),
                            ensure_markers(Es, Subs);
                        _ -> Subs
                    end,
            proxy_loop(Subs1, dict:append(app_started, Info, Events));
        Other ->
            io:fwrite("Proxy got ~p~n", [Other]),
            proxy_loop(Subs, Events)
    end.

call_proxy(N, Req) ->
    call_proxy(N, Req, ?PROXY_CALL_RETRIES, 3000).

call_proxy(N, Req, Tries, Timeout) when Tries > 0 ->
    Ref = erlang:monitor(process, {?PROXY, N}),
    {?PROXY, N} ! {self(), Ref, Req},
    receive
        {'DOWN', Ref, _, _, noproc} ->
            ct:log("proxy not yet there, retrying in 0.1 sec...", []),
            receive
            after 100 ->
                    call_proxy(N, Req, Tries-1, Timeout)
            end;
        {'DOWN', Ref, _, _, Reason} ->
            error({proxy_died, N, Reason});
        {Ref, Result} ->
            erlang:demonitor(Ref),
            Result
    after Timeout ->
            error(proxy_call_timeout)
    end;
call_proxy(N, _, _, _) ->
    erlang:error({proxy_not_running, N}).

events() -> [block_created, micro_block_created, chain_sync, top_changed].

tell_subscribers(Subs, Event, Msg) ->
    lists:foreach(
      fun({P, E}) when E =:= Event, is_pid(P) ->
              P ! Msg;
         (_) ->
              ok
      end, Subs).

set_subscriptions() ->
    Es = events(),
    [aec_events:subscribe(E) || E <- Es],
    Es.

ensure_markers(Es, Subs) ->
    lists:foldl(
      fun(E, Acc) ->
              case lists:member({marker, E}, Acc) of
                  true ->
                      Acc;
                  false ->
                      [{marker, E}|Acc]
              end
      end, Subs, Es).

await_new_jobs_pid(N, OldP) ->
    TRef = erlang:start_timer(5000, self(), await_jobs_pid),
    await_new_jobs_pid(N, OldP, TRef).

await_new_jobs_pid(N, OldP, TRef) ->
    case rpc:call(N, erlang, whereis, [jobs_server]) of
        OldP      -> await_new_jobs_pid_recurse(N, OldP, TRef);
        undefined -> await_new_jobs_pid_recurse(N, OldP, TRef);
        NewP when is_pid(NewP) ->
            erlang:cancel_timer(TRef),
            NewP
    end.

await_new_jobs_pid_recurse(N, OldP, TRef) ->
    receive
        {timeout, TRef, _} ->
            erlang:error(timeout)
    after 100 ->
            await_new_jobs_pid(N, OldP, TRef)
    end.

use_api(SpecVsn) ->
    Prefix =
        case SpecVsn of
            oas3 -> "/v3/";
            rosetta -> "/"
        end,
    put(api_prefix, Prefix).

get(Key, Default) ->
    case get(Key) of
        undefined -> Default;
        V -> V
    end.

http_api_prefix() ->
    get(api_prefix, "/v3/").

http_request(Host, get, Path, Params) ->
    Prefix = get(api_prefix, "/v3/"),
    URL = binary_to_list(
            iolist_to_binary([Host, Prefix, Path, encode_get_params(Params)])),
    ct:log("GET ~p", [URL]),
    R = httpc_request(get, {URL, []}, [], []),
    process_http_return(R);
http_request(Host, post, Path, Params) ->
    Prefix = get(api_prefix, "/v3/"),
    URL = binary_to_list(iolist_to_binary([Host, Prefix, Path])),
    {Type, Body} = case Params of
                       Map when is_map(Map) ->
                           %% JSON-encoded
                           {"application/json", jsx:encode(Params)};
                       [] ->
                           {"application/x-www-form-urlencoded",
                            aeu_uri:encode(Path)}
                   end,
    ct:log("POST ~p, type ~p, Body ~p", [URL, Type, Body]),
    R = httpc_request(post, {URL, [], Type, Body}, [], []),
    process_http_return(R);
http_request(Host, delete, Path, Params) ->
    Prefix = get(api_prefix, "/v2/"),
    URL = binary_to_list(
            iolist_to_binary([Host, Prefix, Path, encode_get_params(Params)])),
    ct:log("DELETE ~p", [URL]),
    R = httpc_request(delete, {URL, []}, [], []),
    process_http_return(R).

httpc_request(Method, Request, HTTPOptions, Options) ->
    httpc_request(Method, Request, HTTPOptions, Options, test_browser).

httpc_request(Method, Request, HTTPOptions, Options, Profile) ->
    {ok, Pid} = inets:start(httpc, [{profile, Profile}], stand_alone),
    Response = httpc:request(Method, Request, HTTPOptions, Options, Pid),
    ok = gen_server:stop(Pid, normal, infinity),
    Response.

encode_get_params(#{} = Ps) ->
    encode_get_params(maps:to_list(Ps));
encode_get_params([{K,V}|T]) ->
    Enc =
        fun(Key, Value) ->
            case Value of
                true -> str(Key);
                _ -> [str(Key), "=", uenc(Value)]
            end
        end,
    lists:flatten(
        ["?", [Enc(K, V)
              | [["&", Enc(K1, V1)]
                  || {K1, V1} <- T]]]);
encode_get_params([]) ->
    [].

str(A) when is_atom(A) ->
    str(atom_to_binary(A, utf8));
str(S) when is_list(S); is_binary(S) ->
    S.

uenc(B) when is_boolean(B) ->
    uenc(atom_to_list(B));
uenc(I) when is_integer(I) ->
    uenc(integer_to_list(I));
uenc(V) ->
    aeu_uri:encode(V).

process_http_return(R) ->
    case R of
        {ok, {{_, ReturnCode, _State}, _Head, Body}} ->
            try
                ct:log("Return code ~p, Body ~p", [ReturnCode, Body]),
                Result =
                    case iolist_to_binary(Body) of
                        <<>> ->
                            #{};
                        BodyB ->
                            jsx:decode(BodyB, [return_maps])
                    end,
                {ok, ReturnCode, Result}
            catch error:E:ST ->
                {error, {parse_error, [E, ST]}}
            end;
        {error, _} = Error ->
            Error
    end.

internal_address() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"http">>, <<"internal">>, <<"port">>],
                aehttp, [internal, port], 8143]),
    "http://127.0.0.1:" ++ integer_to_list(Port).

external_address() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"http">>, <<"external">>, <<"port">>],
                aehttp, [external, port], 8043]),
    "http://127.0.0.1:" ++ integer_to_list(Port).     % good enough for requests

rosetta_address() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"http">>, <<"rosetta">>, <<"port">>],
                aehttp, [rosetta, port], 8243]),
    "http://127.0.0.1:" ++ integer_to_list(Port).

rosetta_offline_address() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"http">>, <<"rosetta_offline">>, <<"port">>],
                aehttp, [rosetta_offline, port], 8343]),
    "http://127.0.0.1:" ++ integer_to_list(Port).

rpc(Mod, Fun, Args) ->
    rpc(?DEFAULT_NODE, Mod, Fun, Args).

rpc(Node, Mod, Fun, Args) ->
    case rpc:call(node_name(Node), Mod, Fun, Args, 5000) of
        {badrpc, Reason} ->
            error({badrpc, Reason});
        R -> R
    end.

generate_key_pair() ->
    #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
    {Pubkey, Privkey}.

meta_tx(Owner, AuthOpts, AuthData, InnerTx0) ->
    InnerSTx =
        try aetx_sign:tx(InnerTx0) of
            _Tx -> InnerTx0
        catch _:_ ->
            aetx_sign:new(InnerTx0, [])
        end,
    Options1 = maps:merge(#{auth_data => AuthData, tx => InnerSTx}, AuthOpts),
    MetaTx   = aega_test_utils:ga_meta_tx(Owner, Options1),
    aetx_sign:new(MetaTx, []).

block_peer(Node, PeerNode) ->
    {ok, PeerInfo} = aec_peers:parse_peer_address(peer_info(PeerNode)),
    rpc(Node, aec_peers, block_peer, [PeerInfo]).

unblock_peer(Node, PeerNode) ->
    rpc(Node, aec_peers, unblock_peer, [pubkey(PeerNode)]).

get_key_hash_by_delta(Node, Delta) ->
    TopHeader = rpc(Node, aec_chain, top_header, []),
    TopHeight = aec_headers:height(TopHeader),
    {ok, Header} = rpc(Node, aec_chain, get_key_header_by_height, [TopHeight - Delta]),
    {ok, Hash} = aec_headers:hash_header(Header),
    Hash.

assert_stopped(Node) ->
    stopped = rpc:call(Node, aec_conductor, get_mining_state, [], 5000).

subscribe_created(Node) ->
    ok = subscribe(Node, block_created),
    ok = subscribe(Node, micro_block_created).

start_mining(Node, Opts) ->
    StartRes = rpc:call(Node, aec_conductor, start_mining, [Opts], 5000),
    ct:log("aec_conductor:start_mining(~p) (~p) -> ~p", [Opts, Node, StartRes]).

stop_mining(Node) ->
    StopRes = rpc:call(Node, aec_conductor, stop_mining, [], 5000),
    ct:log("aec_conductor:stop_mining() (~p) -> ~p", [Node, StopRes]).

unsubscribe_created(Node) ->
    ok = unsubscribe(Node, block_created),
    ok = unsubscribe(Node, micro_block_created).

hc_mine_blocks(Nodes, NBlocks, Timeout, Opts) ->
    [ assert_stopped(Node) || Node <- Nodes ],

    _ = flush_new_blocks(), %% Flush old messages

    [ subscribe_created(Node) || Node <- Nodes ],
    [ start_mining(Node, Opts) || Node <- Nodes ],

    Res = hc_loop_mine_blocks(NBlocks, Timeout),

    [ stop_mining(Node) || Node <- Nodes ],
    [ unsubscribe_created(Node) || Node <- Nodes ],

    [ assert_stopped(Node) || Node <- Nodes ],

    _ = flush_new_blocks(), %% Flush late messages

    case Res of
        {ok, BlocksReverse} ->
            {ok, lists:reverse(BlocksReverse)};
        {error, Reason} ->
            erlang:error(Reason)
    end.

mine_safe_setup(Node, MiningRate, Opts, LoopFun) ->
    ok = rpc:call(Node, application, set_env,
                  [aecore, expected_mine_rate, MiningRate], 5000),

    %% Ensure we are not mining before starting
    stopped = rpc:call(Node, aec_conductor, get_mining_state, [], 5000),
    _ = flush_new_blocks(), %% flush potential hanging message queue messages

    %% Start mining
    ok = subscribe(Node, block_created),
    ok = subscribe(Node, micro_block_created),
    StartRes = rpc:call(Node, aec_conductor, start_mining, [Opts], 5000),
    ct:log("aec_conductor:start_mining(~p) (~p) -> ~p", [Opts, Node, StartRes]),

    %% Run custom loop which is expected to return when done
    Res = LoopFun(),

    %% Stop mining
    StopRes = rpc:call(Node, aec_conductor, stop_mining, [], 5000),
    ct:log("aec_conductor:stop_mining() (~p) -> ~p", [Node, StopRes]),
    ok = unsubscribe(Node, block_created),
    ok = unsubscribe(Node, micro_block_created),

    %% Ensure we are not mining after we've stopped
    stopped = rpc:call(Node, aec_conductor, get_mining_state, [], 5000),
    _ = flush_new_blocks(), %% flush potential hanging message queue messages

    case Res of
        {ok, BlocksReverse} ->
            {ok, lists:reverse(BlocksReverse)};
        {error, Reason} ->
            erlang:error(Reason)
    end.

%%%=============================================================================
%%% Partially imported from https://github.com/erlware/relx/blob/master/src/rlx_util.erl#L232
%%%=============================================================================

symlink_or_copy(Source, Target) ->
    case file:make_symlink(Source, Target) of
        ok ->
            ok;
        {error, eexist} ->
            {error, eexist};
        {error, Err} ->
            case {os:type(), Err} of
                {{win32, _}, eperm} ->
                    % We get eperm on Windows if we do not have
                    %   SeCreateSymbolicLinkPrivilege
                    % Try the next alternative
                    win32_make_junction_or_copy(Source, Target);
                _ ->
                    % On other systems we try to copy next
                    cp_r(Source, Target)
            end
    end.

cp_r(Source, Target) ->
    ec_file:copy(Source, Target, [{recursive, true}, {fileinfo, [mode, time, owner, group]}]).

win32_make_junction_or_copy(Source, Target) ->
    case filelib:is_dir(Source) of
        true ->
            win32_make_junction(Source, Target);
        _ ->
            cp_r(Source, Target)
    end.

win32_make_junction(Source, Target) ->
    % The mklink will fail if the target already exists, check for that first
    case file:read_link_info(Target) of
        {error, enoent} ->
            win32_make_junction_cmd(Source, Target);
        {ok, #file_info{type = symlink}} ->
            case file:read_link(Target) of
                {ok, Source} ->
                    ok;
                {ok, _} ->
                    ok = file:del_dir(Target),
                    win32_make_junction_cmd(Source, Target);
                {error, Reason} ->
                    {error, {readlink, Reason}}
            end;
        {ok, #file_info{type = _Type}} ->
            % Directory already exists, so we overwrite the copy
            cp_r(Source, Target);
        Error ->
            Error
    end.

win32_make_junction_cmd(Source, Target) ->
    S = unicode:characters_to_list(Source),
    T = unicode:characters_to_list(Target),
    Cmd = "cmd /c mklink /j " ++ filename:nativename(T) ++ " " ++ filename:nativename(S),
    case os:cmd(Cmd) of
        "Junction created " ++ _ ->
            ok;
        [] ->
            % When mklink fails it prints the error message to stderr which
            % is not picked up by os:cmd() hence this case switch is for
            % an empty message
            cp_r(Source, Target)
    end.

win32_delete_junction(Target) ->
    T = unicode:characters_to_list(Target),
    Cmd = "cmd /c rd /s /q " ++ filename:nativename(T),
    case os:cmd(Cmd) of
        [] ->
            ok;
        Output ->
            ct:log("Output from deleting junction ~s: ~p~n", [Target, Output]),
            ok
    end.

set_node_name(Node, Path) ->
    Name = atom_to_binary(node_name(Node), utf8),
    ct:log("Setting name ~p to ~p", [Name, Path]),
    {ok, Data} = file:read_file(Path),
    Lines = binary:split(Data, [<<"\n">>], [global]),
    Lines1 =
        lists:map(
            fun(<<"-sname ", _/binary>>) ->
                <<"-sname ", Name/binary>>;
               (Other) -> Other
            end,
            Lines),
    Data1 = binary_join(Lines1, <<"\n">>),
    file:write_file(Path, Data1).

binary_join([First|T], Sep) ->
    binary_join(T, First, Sep).

binary_join([], Accum, _Sep) ->
    Accum;
binary_join([H|T], Accum, Sep) ->
    binary_join(T, <<Accum/binary, Sep/binary, H/binary>>, Sep).


