%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Service storing blocks (and state trees) to disc.
%%%      (Might also store the transaction pool and peers in the future.)
%%%
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aec_persistence).

-behaviour(gen_server).


-export([start_link/0,
         start_link/1,
         stop/0,
         stop_and_clean/0]).

%% API
-export([get_chain/0,
         get_block/1,
         get_block_state/1,
         get_header/1,
         get_top_block/0,
         get_top_header/0,
         path/0,
         sync/0,
         write_block/1,
         write_block_state/2,
         write_header/1,
         write_top_block/1,
         write_top_header/1
        ]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("common.hrl").
-include("blocks.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API -  the main API for the server is in aec_chain.
%%%===================================================================

start_link() ->
    Path =
        case  application:get_env(db_path) of
            {ok, DBPath} ->
                filename:join(DBPath, ".chain");
            _ ->
                ".chain"
        end,
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Path], []).

start_link(Path) ->
    ChainPath = filename:join(Path, ".chain"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ChainPath], []).

stop() ->
    gen_server:stop(?SERVER).

%% WARNING to be used by tests. Deletes the whole DB before stoping.
stop_and_clean() ->
    Path = path(),
    os:cmd("rm -rf " ++ Path),
    stop().

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-record(aecp_state, {path = ".chain"
                    , blocks_db = ".chain/blocks"
                    , top_header_hash = ".chain/top_header"
                    , top_block_hash  = ".chain/top_block"
                    , state_db = ".chain/states"}).

init(_Args = [Path]) ->
    init(init_state(Path));
init(State) when is_record(State, aecp_state) ->
    #aecp_state{path = Path} = State,
    case file:list_dir(Path) of
        {error, enoent} -> init_dir(State);
        {ok,_Files} -> {ok, State}
    end.

init_state(Path) ->
    _State = #aecp_state{path = Path
                        , blocks_db       = filename:join(Path, "blocks")
                        , top_header_hash = filename:join(Path, "top_header")
                        , top_block_hash  = filename:join(Path, "top_block")
                        , state_db        = filename:join(Path, "states")
                        }.

init_dir(#aecp_state{path = Path
                    , blocks_db = Blocks
                    , top_header_hash = Header
                    , top_block_hash  = Block
                    , state_db = States} = S) ->
    case application:get_env(aecore, persist, false) of
        true ->
            ok = file:make_dir(Path),
            ok = file:make_dir(Blocks),
            ok = file:make_dir(States),
            ok = file:write_file(Header,
                                 term_to_binary(undefined, [compressed])),
            ok = file:write_file(Block,
                                 term_to_binary(undefined, [compressed])),
            {ok, S};
        false ->
            {ok, S}
    end.

%%%===================================================================
%%% API
%%%===================================================================

sync() ->  gen_server:call(?SERVER, sync).
path() ->  gen_server:call(?SERVER, path).
get_chain() ->  gen_server:call(?SERVER, get_chain).
write_block(Block) -> gen_server:call(?SERVER, {write_block, Block}).
write_block_state(Hash, State) ->
    gen_server:call(?SERVER, {write_block_state, Hash, State}).
write_header(Header) ->
    gen_server:call(?SERVER, {write_header, Header}).
write_top_block(Hash) ->
    gen_server:call(?SERVER, {write_top_block, Hash}).
write_top_header(Hash) ->
    gen_server:call(?SERVER, {write_top_header, Hash}).
get_block(Hash) ->
    gen_server:call(?SERVER, {get_block, Hash}).
get_header(Hash) ->
    gen_server:call(?SERVER, {get_header, Hash}).
get_top_block() ->
    gen_server:call(?SERVER, get_top_block).
get_top_header()  ->
    gen_server:call(?SERVER, get_top_header).
get_block_state(Hash) ->
    gen_server:call(?SERVER, {get_block_state, Hash}).

%% State preserving functions
handle_call(sync, _From, State) ->
    %% Ensure that the persistense server has caught up with writing.
    {reply, ok, State};
handle_call(path, _From, #aecp_state{path = Path} = State) ->
    %% Ensure that the persistense server has caught up with writing.
    {reply, Path, State};
handle_call(get_chain, _From, State) ->
    Chain = int_get_chain(State),
    {reply, Chain, State};
handle_call({write_block, Block}, _From, State) ->
    {reply, int_write_block(Block, State), State};
handle_call({write_block_state, Hash, S}, _From, State) ->
    {reply, int_write_block_state(Hash, S, State), State};
handle_call({write_header, Header}, _From, State) ->
    {reply, int_write_header(Header, State), State};
handle_call({write_top_block, Hash}, _From, State) ->
    {reply, int_write_top_block(Hash, State), State};
handle_call({write_top_header, Hash}, _From, State) ->
    {reply, int_write_top_header(Hash, State), State};
handle_call({get_block, Hash}, _From, State) ->
    {reply, int_get_block(Hash, State), State};
handle_call({get_header, Hash}, _From, State) ->
    {reply, int_get_header(Hash, State), State};
handle_call(get_top_block, _From, State) ->
    {reply, int_get_top_block(State), State};
handle_call(get_top_header, _From, State) ->
    {reply, int_get_top_header(State), State};
handle_call({get_block_state, Hash}, _From, State) ->
    {reply, int_get_block_state(Hash, State), State};

handle_call(Request, From, State) ->
    lager:warning("Unknown call request from ~p: ~p", [From, Request]),
    {reply, {error, unknown_request}, State}.

handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Ignoring unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

int_write_block(Block, State) ->
    Hash = hash(Block),
    File = aec_blocks:serialize_for_store(Block),
    write_block(Hash, File, State).

int_write_header(Header, State) ->
    Hash = hash(Header),
    File = aec_headers:serialize_for_store(Header),
    write_header(Hash, File, State).

int_write_block_state(Hash, S, State) ->
    File = serialize(S),
    write_state(Hash, File, State).

int_write_top_block(Hash, #aecp_state{top_block_hash = FileName}) ->
    write(FileName, serialize(Hash)).

int_write_top_header(Hash, #aecp_state{top_header_hash = FileName}) ->
    write(FileName, serialize(Hash)).

int_get_block(Hash, State) ->
    FileName = block_filename(Hash, State),
    {ok, Binary} = file:read_file(FileName),
    {ok, Block} = aec_blocks:deserialize_from_store(Binary),
    Block.

int_get_header(Hash, State) ->
    FileName = header_filename(Hash, State),
    {ok, Binary} = file:read_file(FileName),
    {ok, Header} = aec_headers:deserialize_from_store(Binary),
    Header.


int_get_top_block(#aecp_state{top_block_hash = FileName}) ->
    {ok, Binary} = file:read_file(FileName),
    deserialize(Binary).
int_get_top_header(#aecp_state{top_header_hash = FileName}) ->
    {ok, Binary} = file:read_file(FileName),
    deserialize(Binary).
int_get_block_state(Hash, State) ->
    FileName = state_filename(Hash, State),
    {ok, Binary} = file:read_file(FileName),
    StateTrees = deserialize(Binary),
    StateTrees.

int_get_chain(#aecp_state{blocks_db = Path}) ->
    ReadFun = fun(FileName, Acc) ->
                      case get_block_or_header(FileName) of
                          undefined -> Acc;
                          Blob -> [Blob|Acc]
                      end
              end,
    Chain = filelib:fold_files(Path, ".*", true, ReadFun, []),
    Chain.

get_block_or_header(FileName) ->
    {ok, Binary} = file:read_file(FileName),
    case aec_blocks:deserialize_from_store(Binary) of
        {ok, Block} -> Block;
        false ->
            case aec_headers:deserialize_from_store(Binary) of
                {ok, Header} -> Header;
                false -> undefined
            end
    end.

serialize(T) -> term_to_binary(T, [{compressed,9}]).
deserialize(B) -> binary_to_term(B).

write_block(Hash, Bin, State) ->
    write(block_filename(Hash, State), Bin).

write_header(Hash, Bin, State) ->
    FileName = header_filename(Hash, State),
    %% Don't overwrite a potential block file with header.
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
            write(header_filename(Hash, State), Bin)
    end.

write_state(Hash, Bin, State) ->
    write(state_filename(Hash, State), Bin).

hash(#block{} = B) ->
    {ok, Hash} = aec_headers:hash_header(aec_blocks:to_header(B)),
    Hash;
hash(#header{} = H) ->
    {ok, Hash} = aec_headers:hash_header(H),
    Hash.

header_filename(<<A:8,B:8,Hash/binary>>, #aecp_state{blocks_db = Path}) ->
    mk_filename(Path, <<A, B>>, Hash).
block_filename(<<A:8,B:8,Hash/binary>>, #aecp_state{blocks_db = Path}) ->
    mk_filename(Path, <<A, B>>, Hash).
state_filename(<<A:8,B:8,Hash/binary>>, #aecp_state{state_db = Path}) ->
    mk_filename(Path, <<A, B>>, Hash).

mk_filename(Path, DirBin, Hash) ->
    Dir = to_hexstring(DirBin),
    DirPath = filename:join(Path, Dir),
    ok = ensure_dir_exists(DirPath),
    filename:join(DirPath, to_hexstring(Hash)).

write(FileName, Bin) ->
    case application:get_env(aecore, persist, false) of
        true ->
            ok = file:write_file(FileName, Bin);
        false ->
            ok
    end.

ensure_dir_exists(Path) ->
    case application:get_env(aecore, persist, false) of
        true ->
            case file:list_dir(Path) of
                {error, enoent} ->
                    ok = file:make_dir(Path);
                {ok,_Files} -> ok
            end;
        false ->
            ok
    end.

to_hexstring(B) when is_binary(B) ->
    [io_lib:format("~2.16.0B", [C]) || C <- binary_to_list(B)].
