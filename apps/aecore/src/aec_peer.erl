-module(aec_peer).

%% API
-export([new/4,
         socket/2
        ]).

%% getters
-export([ id/1
        , pubkey/1
        , host/1
        , ip/1
        , port/1
        , socket/1
        , is_trusted/1
        , ppp/1
        , info/1
        , info/3
        , format_address/1
        , source/1
        , set_source/2
        ]).

-ifdef(TEST).
-export([ set_trusted/2,
          peer_config_info/1 ]).
-endif.

-record(peer, {
    pubkey            :: aec_keys:pubkey(),
    host              :: host(),
    address           :: inet:ip_address(),
    source            :: inet:ip_address(),
    port              :: inet:port_number(),
    % If it is a pre-configured peer.
    trusted = false   :: boolean()
}).

-record(socket, {
    address           :: inet:ip_address(),
    port              :: inet:port_number()
}).

%% The Peer is identified by its pubkey for now.
-type info() :: #{
    pubkey  => aec_keys:pubkey(),
    host    => host(),
    port    => inet:port_number()
}.
-type id() :: binary().
-type peer() :: #peer{}.
-type socket() :: #socket{}.
-type host() :: string() | binary().


-export_type([id/0,
              peer/0,
              info/0,
              socket/0]).


-spec new(inet:ip_address(), inet:ip_address(), aec_peer:info(), boolean()) -> peer().
new(PeerAddr, Source, PeerInfo, IsTrusted) ->
    #{ pubkey := PubKey, host := Host, port := Port } = PeerInfo,
    #peer{
        pubkey = PubKey,
        host = Host,
        address = PeerAddr,
        source = Source,
        port = Port,
        trusted = IsTrusted
    }.


-spec id(peer() | info()) -> id().
id(#peer{pubkey = Pubkey}) ->
    Pubkey;
id(#{ pubkey := PubKey }) ->
    PubKey.

-spec socket(peer()) -> socket().
socket(#peer{address = Address,
             port    = Port}) ->
    socket(Address, Port).

socket(Address, Port) ->
    #socket{address = Address, port = Port}.

-spec ppp(id() | peer() | info()) -> string().
ppp(PeerId) when is_binary(PeerId) ->
    Str = lists:flatten(io_lib:format("~p", [aeser_api_encoder:encode(peer_pubkey, PeerId)])),
    lists:sublist(Str, 4, 10) ++ "..." ++ lists:sublist(Str, length(Str) - 9, 7);
ppp(#{ pubkey := PK }) -> ppp(PK);
ppp(#peer{ pubkey = PK }) -> ppp(PK);
ppp(X) -> X.

%% Gets peer's info from given data.
-spec info(peer()) -> info().
info(#peer{ host = H, port = P, pubkey = PK }) ->
    #{ host => H, port => P, pubkey => PK }.

info(PK, Host, Port) when is_list(Host) ->
    info(PK, list_to_binary(Host), Port);
info(PK, Host, Port) when is_binary(Host) ->
    #{ host => Host, port => Port, pubkey => PK }.

-spec format_address(peer() | inet:ip_address() | {inet:ip_address(), inet:port_number()})
    -> binary().
format_address({A, B, C, D}) ->
    iolist_to_binary(io_lib:format("~b.~b.~b.~b", [A, B, C, D]));
format_address({{A, B, C, D}, P}) ->
    iolist_to_binary(io_lib:format("~b.~b.~b.~b:~b", [A, B, C, D, P]));
format_address(#peer{ address = Addr, port = Port }) ->
    format_address({Addr, Port}).

-spec pubkey(peer() | info()) -> aec_keys:pubkey().
pubkey(#peer{ pubkey = Pubkey }) -> Pubkey;
pubkey(#{ pubkey := Pubkey }) -> Pubkey.

-spec ip(peer() | info()) -> inet:ip_address().
ip(#peer{ address = Address }) -> Address;
ip(#{ address := Address }) -> Address.

-spec host(peer()) -> host().
host(#peer{ host = Host }) -> Host.

-spec port(peer() | info()) -> inet:port_number().
port(#peer{ port = Port }) -> Port;
port(#{ port := Port }) -> Port.

-spec is_trusted(peer()) -> boolean().
is_trusted(#peer{ trusted = Trusted }) -> Trusted.

-ifdef(TEST).
set_trusted(Peer, Trusted) ->
    Peer#peer{trusted = Trusted}.

peer_config_info(PeerOrInfo) ->
    PK = pubkey(PeerOrInfo),
    EncodedPK = aeser_api_encoder:encode(peer_pubkey, PK),
    FormattedAddr = format_address(PeerOrInfo),
    <<"aenode://", EncodedPK/binary, "@", FormattedAddr/binary>>.

-endif.

-spec source(peer()) -> inet:ip_address().
source(#peer{source = Source}) ->
    Source.

-spec set_source(peer(), inet:ip_address()) -> peer().
set_source(Peer, Source) ->
    Peer#peer{source = Source}.

