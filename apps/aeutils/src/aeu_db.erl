%%%=============================================================================
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%    Helpers to change node name in Mnesia
%%% @end
%%%=============================================================================
-module(aeu_db).

%% API
-export([change_node/1]).

%% TODO: Backup SCHEMA.DAT before starting dets manipulations!
-define(DEFAULT_FROM_NAME, 'epoch@localhost').
-define(DEFAULT_TO_NAME, 'aeternity@localhost').
-define(db_renaming_error(E), {db_renaming_error, E}).

%%%===================================================================
%%% API
%%%===================================================================

change_node(SchemaDATFilePath) ->
    change_node(?DEFAULT_FROM_NAME, ?DEFAULT_TO_NAME, SchemaDATFilePath).

change_node(FromNode, ToNode, SchemaDATFilePath) ->
    try do_change_node(FromNode, ToNode, SchemaDATFilePath)
    catch throw:?db_renaming_error(Error) -> {error, Error}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_change_node(FromNode, ToNode, SchemaDatFile) ->
    ok   = assert_file_present(SchemaDatFile),
    Name = dets_open_file(SchemaDatFile),
    ok   = dets_change_node(FromNode, ToNode, Name),
    ok   = dets_sync(Name),
    ok   = dets_close(Name).

assert_file_present(SchemaDatFile) ->
    case filelib:is_regular(SchemaDatFile) of
        true -> ok;
        false ->
            io:fwrite("Schema file not found: ~p~n", [SchemaDatFile]),
            db_renaming_error(file_not_found)
    end.

dets_open_file(SchemaDatFile) ->
    case dets:open_file(schema, [{file, SchemaDatFile}, {repair, false}, {keypos, 2}]) of
        {ok, Name} -> Name;
        {error, Reason} ->
            io:fwrite("Opening SCHEMA.DAT file failed: ~p~n", [Reason]),
            db_renaming_error(Reason)
    end.

dets_change_node(FromNode, ToNode, Name) ->
    dets_change_node(FromNode, ToNode, Name, dets:first(Name)).

dets_change_node(_FromNode, _ToNode, _Name, '$end_of_table') ->
    ok;
dets_change_node(FromNode, ToNode, Name, Key) ->
    case dets:lookup(Name, Key) of
        [{Name, Tab, Def}] ->
            NewDef = rename_node(FromNode, ToNode, Def),
            case dets:insert(Name, {Name, Tab, NewDef}) of
                ok ->
                    dets_change_node(FromNode, ToNode, Name, dets:next(Name, Key));
                {error, Reason} ->
                    io:fwrite("Inserting into SCHEMA.DAT failed: ~p~n", [Reason]),
                    db_renaming_error(Reason)
            end;
        {error, Reason} ->
            io:fwrite("Key ~p lookup in SCHEMA.DAT file failed: ~p", [Key, Reason]),
            db_renaming_error(Reason)
    end.

rename_node(FromNode, ToNode, Def) ->
    ToBeRenamed =
        lists:filter(
          fun({_Backend, [Node]}) when Node =:= FromNode -> true;
             ({cookie, {_, Node}}) when Node =:= FromNode -> true;
             ({version, {_, {Node, _}}}) when Node =:= FromNode -> true;
             (_Other) -> false
          end, Def),
    lists:foldl(
      fun({Backend, [Node]}, Acc) when Node =:= FromNode ->
              lists:keyreplace(Backend, 1, Acc, {Backend, [ToNode]});
         ({cookie, {A, Node}}, Acc) when Node =:= FromNode ->
              lists:keyreplace(cookie, 1, Acc, {cookie, {A, ToNode}});
         ({version, {Vsn, {Node, Ts}}}, Acc) when Node =:= FromNode ->
              lists:keyreplace(version, 1, Acc, {version, {Vsn, {ToNode, Ts}}})
      end, Def, ToBeRenamed).

dets_sync(Name) ->
    case dets:sync(Name) of
        ok -> ok;
        {error, Reason} ->
            io:fwrite("Syncing SCHEMA.DAT file failed: ~p~n", [Reason]),
            db_renaming_error(Reason)
    end.

dets_close(Name) ->
    case dets:close(Name) of
        ok -> ok;
        {error, Reason} ->
            io:fwrite("Closing SCHEMA.DAT file failed: ~p~n", [Reason]),
            db_renaming_error(Reason)
    end.

db_renaming_error(Error) ->
    throw(?db_renaming_error(Error)).
