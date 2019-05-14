-module(aeu_keys_gen_SUITE).

-define(OPS_CMD, "aeternity").

-define(KEYS_DIR, "generated_keys").
-define(PUB, "key.pub").
-define(PRIV, "key").

-define(PWD, <<"foo">>).

-define(PWD_WITH_LSPACE , <<" password_with_space_at_the_left">>).
-define(APWD_WITH_LSPACE,  <<"password_with_space_at_the_left">>). %% Actual password (left space trimmed).

-define(PWD_WITH_MIDSPACE, <<"password_with_space_in_the middle">>).

-define(PWD_WITH_RSPACE , <<"password_with_space_at_the_right ">>).
-define(APWD_WITH_RSPACE, <<"password_with_space_at_the_right">> ). %% Actual password (right space trimmed).

-define(PWD_WITH_SPACES , <<" a pwd  with spaces  ">>).
-define(APWD_WITH_SPACES,  <<"a pwd  with spaces">>  ). %% Actual password (spaced trimmed from left and right, double spaces in the middle are NOT compressed).

-define(NODE, dev1).

%% common_test exports
-export(
   [
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [ plain_pwd/1
   , lspace_pwd/1
   , midspace_pwd/1
   , rspace_pwd/1
   , spaces_pwd/1
   ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [ plain_pwd
    , lspace_pwd
    , midspace_pwd
    , rspace_pwd
    , spaces_pwd
    ].

init_per_suite(Config) ->
    aecore_suite_utils:init_per_suite([?NODE], [{symlink_name, "latest.keys_gen"}, {test_module, ?MODULE}] ++ Config).

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    false = filelib:is_file(generated_keys_dir(?NODE, Config)), %% Hardcoded expectation. Checks file or directory.
    Config.

end_per_testcase(_Case, Config) ->
    ok = delete_generated_keys_dir(generated_keys_dir(?NODE, Config)),
    ok.

%% ============================================================
%% Test cases
%% ============================================================
plain_pwd(Config) ->
    run_and_check_keys_gen(?OPS_CMD, ?PWD, ?PWD, Config).

lspace_pwd(Config) ->
    run_and_check_keys_gen(?OPS_CMD, ?PWD_WITH_LSPACE, ?APWD_WITH_LSPACE, Config).

midspace_pwd(Config) ->
    run_and_check_keys_gen(?OPS_CMD, ?PWD_WITH_MIDSPACE, ?PWD_WITH_MIDSPACE, Config).

rspace_pwd(Config) ->
    run_and_check_keys_gen(?OPS_CMD, ?PWD_WITH_RSPACE, ?APWD_WITH_RSPACE, Config).

spaces_pwd(Config) ->
    run_and_check_keys_gen(?OPS_CMD, ?PWD_WITH_SPACES, ?APWD_WITH_SPACES, Config).

%% ============================================================
%% Helpers
%% ============================================================
generated_keys_dir(N, Config) ->
    filename:join(aecore_suite_utils:node_shortcut(N, Config), ?KEYS_DIR).

delete_generated_keys_dir(D) ->
    case filelib:is_dir(D) of
        false ->
            ok;
        true ->
            aecore_suite_utils:delete_file(filename:join(D, ?PRIV)),
            aecore_suite_utils:delete_file(filename:join(D, ?PUB)),
            {ok, []} = file:list_dir(D),
            ok = file:del_dir(D)
    end.

run_and_check_keys_gen(Cmd, UserPassword, ActualPassword, Config) ->
    run_and_check_keys_gen(?NODE, Cmd, UserPassword, ActualPassword, Config).

run_and_check_keys_gen(N, Cmd, UserPassword, ActualPassword, Config) ->
    KeysDir = generated_keys_dir(N, Config),
    false = filelib:is_file(KeysDir), %% Hardcoded expectation. Checks file or directory.
    %% Run script.
    Res = aecore_suite_utils:cmd(Cmd, aecore_suite_utils:node_shortcut(N, Config), "bin", ["keys_gen", UserPassword]),
    {Code, Out} = aecore_suite_utils:cmd_res(Res),
    %% Check script exit code ok.
    ?assertEqual(0, Code),
    %% Check key files created.
    ?assert(filelib:is_dir(KeysDir)),
    {ok, KeyFiles} = file:list_dir(KeysDir),
    ?assertMatch([_, _], KeyFiles),
    PrivFile = filename:join(KeysDir, ?PRIV),
    true = filelib:is_file(PrivFile),
    PubFile = filename:join(KeysDir, ?PUB),
    true = filelib:is_file(PubFile),
    %% Check pubkey can be extracted from script output.
    {match, [PrettyPubKey]} = re:run(Out, "ak\\_[A-Za-z0-9]*", [{capture, first, binary}]),
    {ok, PubKey} = aeser_api_encoder:safe_decode(account_pubkey, PrettyPubKey),
    %% Check that pubkey written on filesystem with password corresponds to the one printed by script.
    {ok, EncryptedPubKey} = file:read_file(PubFile),
    ?assertEqual(PubKey, decrypt_key(ActualPassword, EncryptedPubKey)),
    if
        ActualPassword =/= UserPassword ->
            %% Check that pubkey written on filesystem decrypted with verbatim user password would not correspond to the one printed by script. (Otherwise, why would caller specify an actual password distinct from the user password?)
            ?assertNotEqual(PubKey, decrypt_key(UserPassword, EncryptedPubKey));
        true ->
            ok
    end,
    %% TODO Check private key.
    ok.

decrypt_key(Password, Bin) ->
    crypto:block_decrypt(aes_ecb, crypto:hash(sha256, Password), Bin).
