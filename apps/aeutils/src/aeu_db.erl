%%%=============================================================================
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%    Helpers to change node name in Mnesia
%%% @end
%%%=============================================================================
-module(aeu_db).

%% API
-export([change_node/3]).

-define(db_renaming_error(E), {db_renaming_error, E}).

%%%===================================================================
%%% API
%%%===================================================================

change_node(SchemaDATFilePath, FromNode, ToNode) when is_list(SchemaDATFilePath)
                                                      andalso is_atom(FromNode)
                                                      andalso is_atom(ToNode) ->
    try do_change_node(SchemaDATFilePath, FromNode, ToNode)
    catch throw:?db_renaming_error(Error) -> {error, Error}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_change_node(SchemaDatFile, FromNode, ToNode) ->
    ok      = assert_file_present(SchemaDatFile),
    ok      = prepare_schema_dat_backup(SchemaDatFile),
    TabName = dets_open_file(SchemaDatFile),
    ok      = dets_change_node(FromNode, ToNode, TabName),
    ok      = dets_sync(TabName),
    ok      = dets_close(TabName).

assert_file_present(SchemaDatFile) ->
    case filelib:is_regular(SchemaDatFile) of
        true -> ok;
        false ->
            io:fwrite("Schema file not found: ~p~n", [SchemaDatFile]),
            db_renaming_error(file_not_found)
    end.

prepare_schema_dat_backup(SchemaDatFile) ->
    BackupFile = get_backup_file(SchemaDatFile),
    case file:copy(SchemaDatFile, {BackupFile, [sync]}) of
        {ok, _} ->
            io:fwrite("Created schema.DAT backup file at: ~p. "
                      "Please restore from it if renaming process is interrupted or fails.~n", [BackupFile]),
            ok;
        {error, Reason} ->
            io:fwrite("Error preparing schema.DAT backup: ~p~n", [Reason]),
            db_renaming_error(Reason)
    end.

get_backup_file(SchemaDatFile) ->
    BackupFile = lists:concat([SchemaDatFile, ".backup"]),
    case file_exists(BackupFile) of
        true -> find_nonexisting_file(SchemaDatFile);
        false -> BackupFile
    end.

file_exists(File) ->
    case file:read_link_info(File) of
        {ok, _} -> true;
        _ -> false
    end.

find_nonexisting_file(SchemaDatFile) ->
    Ts = erlang:system_time(millisecond),
    BackupFile = lists:concat([SchemaDatFile, ".", Ts, ".backup"]),
    case file_exists(BackupFile) of
        true -> find_nonexisting_file(SchemaDatFile);
        false -> BackupFile
    end.

dets_open_file(SchemaDatFile) ->
    case dets:open_file(schema, [{file, SchemaDatFile}, {repair, false}, {keypos, 2}]) of
        {ok, Name} -> Name;
        {error, Reason} ->
            io:fwrite("Opening schema.DAT file failed: ~p~n", [Reason]),
            db_renaming_error(Reason)
    end.

dets_change_node(FromNode, ToNode, TabName) ->
    dets_change_node(FromNode, ToNode, TabName, dets:first(TabName)).

dets_change_node(_FromNode, _ToNode, _TabName, '$end_of_table') ->
    ok;
dets_change_node(FromNode, ToNode, TabName, Key) ->
    case dets:lookup(TabName, Key) of
        [{TabName, Tab, Def}] ->
            NewDef = rename_node(FromNode, ToNode, Def),
            case dets:insert(TabName, {TabName, Tab, NewDef}) of
                ok ->
                    dets_change_node(FromNode, ToNode, TabName, dets:next(TabName, Key));
                {error, Reason} ->
                    io:fwrite("Inserting into schema.DAT failed: ~p~n", [Reason]),
                    db_renaming_error(Reason)
            end;
        {error, Reason} ->
            io:fwrite("Key ~p lookup in schema.DAT file failed: ~p", [Key, Reason]),
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

dets_sync(TabName) ->
    case dets:sync(TabName) of
        ok -> ok;
        {error, Reason} ->
            io:fwrite("Syncing schema.DAT file failed: ~p~n", [Reason]),
            db_renaming_error(Reason)
    end.

dets_close(TabName) ->
    case dets:close(TabName) of
        ok -> ok;
        {error, Reason} ->
            io:fwrite("Closing schema.DAT file failed: ~p~n", [Reason]),
            db_renaming_error(Reason)
    end.

db_renaming_error(Error) ->
    throw(?db_renaming_error(Error)).
