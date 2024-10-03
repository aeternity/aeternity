%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity Anstalt
%%% @doc Parent chain connector behavior
%%%
%%% @end
-module(aehttpc).

-export([ parse_node_url/1
        , url/1 ]).


-type node_spec() :: #{ host     := string()
                      , scheme   := string()
                      , port     => non_neg_integer()
                      , user     => binary()
                      , password => binary()}.
-type seed() :: binary().
-type hash() :: binary().
-type height() :: non_neg_integer().
-type time() :: non_neg_integer().
-type pubkey() :: binary().
-type chain() :: aeternity | btc | doge.

-export_type([ node_spec/0 ]).

-callback get_latest_block(node_spec(), seed()) ->
    {ok, hash(), hash(), height(), time()} | {error, term()}.

-callback get_header_by_hash(hash(), node_spec(), seed()) ->
    {ok, hash(), hash(), height(), time()} | {error, term()}.

-callback get_header_by_height(height(), node_spec(), seed()) ->
    {ok, hash(), hash(), height(), time()} | {error, term()}.

-callback get_chain_type() ->
    {ok, chain()}.

-spec parse_node_url(string()) -> node_spec().
parse_node_url(NodeURL) ->
    URIMap = uri_string:parse(NodeURL),
    maps:from_list(
      [ {host, maps:get(host, URIMap, "localhost")}
      , {scheme, maps:get(scheme, URIMap, "http")} ] ++
      [ {port, maps:get(port, URIMap)} || maps:is_key(port, URIMap) ] ++
      parse_userinfo(maps:get(userinfo, URIMap, ""))
     ).

parse_userinfo("") -> [];
parse_userinfo(UserInfo) ->
    [User, Password] = string:split(UserInfo, ":"),
    [{user, iolist_to_binary(User)}, {password, iolist_to_binary(Password)}].

-spec url(node_spec()) -> string().
url(#{scheme := Scheme, host := Host, port := Port}) ->
    lists:flatten(io_lib:format("~s://~s:~p", [Scheme, Host, Port]));
url(#{scheme := Scheme, host := Host}) ->
    lists:flatten(io_lib:format("~s://~s", [Scheme, Host])).
