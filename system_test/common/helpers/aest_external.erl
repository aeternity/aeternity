-module(aest_external).

% API
-export([start/1]).
-export([stop/1]).
-export([prepare_spec/2]).
-export([peer_from_spec/2]).
-export([setup_node/2]).
-export([delete_node/1]).
-export([start_node/1]).
-export([stop_node/2]).
-export([kill_node/1]).
-export([stop_container/2]).
-export([node_logs/1]).
-export([get_peer_address/1]).
-export([get_service_address/2]).
-export([get_internal_address/2]).
-export([get_node_pubkey/1]).
-export([extract_archive/3]).
-export([run_cmd_in_node_dir/3]).
-export([connect_node/2]).
-export([disconnect_node/2]).
-export([get_log_path/1]).
-export([export/2]).

%--- Macros --------------------------------------------------------------------

-define(EXT_HTTP_PORT, 3013).
-define(EXT_SYNC_PORT, 3015).

%--- API -----------------------------------------------------------------------

start(_Options) -> #{}.

stop(_State) -> ok.

prepare_spec(#{addr := Addr} = Spec, _State) ->
    ExtHTTP = list_to_binary(io_lib:format("http://~s:~b", [Addr, ?EXT_HTTP_PORT])),
    {ok, 200, _, #{pubkey := Key}} =
        aeload_http:req(ExtHTTP, 'GetPeerPubkey', #{}),
    Spec#{pubkey => Key, ext_http => ExtHTTP}.

peer_from_spec(#{addr := Addr, pubkey := Key} = _Spec, _State) ->
    aec_peers:encode_peer_address(#{
        host => Addr,
        port => ?EXT_SYNC_PORT,
        pubkey => Key
    }).

setup_node(Spec, _State) -> _DummyNodeState = Spec.

delete_node(_NodeState) -> ok.

start_node(NodeState) -> NodeState.

stop_node(NodeState, _Opts) -> NodeState.

kill_node(_NodeState) ->
    error({not_supported, ?FUNCTION_NAME}).

stop_container(_NodeState, _Opts) ->
    error({not_supported, ?FUNCTION_NAME}).

node_logs(_NodeState) ->
    error({not_supported, ?FUNCTION_NAME}).

get_peer_address(_NodeState) ->
    error(not_implemented, [_NodeState]).

get_service_address(ext_http, #{ext_http := ExtHTTP}) ->
    ExtHTTP;
get_service_address(int_http, _NodeState) ->
    "";
get_service_address(_Service, _NodeState) ->
    error(not_implemented, [_Service, _NodeState]).

get_internal_address(_Service, _NodeState) ->
    error(not_implemented, [_Service, _NodeState]).

get_node_pubkey(_NodeState) ->
    error(not_implemented, [_NodeState]).

extract_archive(_NodeState, _Path, _Archive) ->
    error({not_supported, ?FUNCTION_NAME}).

run_cmd_in_node_dir(_NodeState, _Cmd, _Opts) ->
    error({not_supported, ?FUNCTION_NAME}).

connect_node(_NetName, _NodeState) ->
    error({not_supported, ?FUNCTION_NAME}).

disconnect_node(_NetName, _NodeState) ->
    error({not_supported, ?FUNCTION_NAME}).

get_log_path(_NodeState) ->
    error(not_implemented, [_NodeState]).

export(_NodeState, _Name) ->
    error({not_supported, ?FUNCTION_NAME}).
