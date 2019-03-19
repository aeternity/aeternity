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

change_node(DbDirPath, FromNode, ToNode) when is_list(DbDirPath)
                                                      andalso is_atom(FromNode)
                                                      andalso is_atom(ToNode) ->
    try do_change_node(DbDirPath, FromNode, ToNode)
    catch throw:?db_renaming_error(Error) -> {error, Error}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_change_node(DbDir0, FromNode, ToNode) ->
    DbDir = filename:join([DbDir0, "mnesia"]),
    ok = change_schema_dat(DbDir, FromNode, ToNode),
    ok = change_latest_log(DbDir, FromNode, ToNode),
    ok.

%%%===================================================================
%%% schema.DAT renaming functions, dets operations
%%%===================================================================

change_schema_dat(DbDir, FromNode, ToNode) ->
    SchemaDatFile = filename:join(DbDir, "schema.DAT"),
    ok      = assert_schema_dat_present(SchemaDatFile),
    ok      = prepare_schema_dat_backup(SchemaDatFile),
    TabName = dets_open_file(SchemaDatFile),
    ok      = dets_change_node(FromNode, ToNode, TabName),
    ok      = dets_sync(TabName),
    ok      = dets_close(TabName).

assert_schema_dat_present(SchemaDatFile) ->
    case filelib:is_regular(SchemaDatFile) of
        true -> ok;
        false ->
            io:fwrite("Schema file not found: ~p~n", [SchemaDatFile]),
            db_renaming_error(file_not_found)
    end.

prepare_schema_dat_backup(SchemaDatFile) ->
    BackupFile = get_nonexisting_file(SchemaDatFile, ".backup"),
    case file:copy(SchemaDatFile, {BackupFile, [sync]}) of
        {ok, _} ->
            io:fwrite("Created schema.DAT backup file at: ~p. "
                      "Please restore from it if renaming process is interrupted or fails.~n", [BackupFile]),
            ok;
        {error, Reason} ->
            io:fwrite("Error preparing schema.DAT backup: ~p~n", [Reason]),
            db_renaming_error(Reason)
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
            NewDef = change_node_in_tab_def(Def, FromNode, ToNode),
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

%%%===================================================================
%%% LATEST.LOG renaming functions, disk_log operations
%%%===================================================================

-define(LATEST_LOG, latest_log).
-define(LATEST_LOG_NEW, latest_log_new).

change_latest_log(DbDir, FromNode, ToNode) ->
    LatestLogFile = filename:join(DbDir, "LATEST.LOG"),
    case filelib:is_regular(LatestLogFile) of
        true ->
            ok = disk_log_open_file(LatestLogFile, ?LATEST_LOG),
            LatestLogNewFile = get_nonexisting_file(LatestLogFile, ".new"),
            ok = disk_log_open_file(LatestLogNewFile, ?LATEST_LOG_NEW),
            ok = write_changed_log_file(?LATEST_LOG, ?LATEST_LOG_NEW, FromNode, ToNode),
            ok = disk_log_sync(?LATEST_LOG_NEW),
            ok = disk_log_close(?LATEST_LOG),
            ok = disk_log_close(?LATEST_LOG_NEW),
            ok = replace_file(LatestLogFile, LatestLogNewFile),
            ok;
        false ->
            io:fwrite("LATEST.LOG file not found, renaming not needed~n")
    end.

disk_log_open_file(File, Name) ->
    case disk_log:open([{file, File}, {name, Name}]) of
        {ok, Name} -> ok;
        {repaired, Name, {recovered, _Rec}, {badbytes, _Bad}} ->
            %% Happens when the renaming is executed when the node is running
            %% (db is in use).
            ok;
        Other ->
            io:fwrite("Opening log file ~p failed: ~p~n", [File, Other]),
            db_renaming_error(Other)
    end.

write_changed_log_file(OldLog, NewLog, FromNode, ToNode) ->
    write_changed_log_file(OldLog, NewLog, FromNode, ToNode, disk_log:chunk(OldLog, start, 1)).

write_changed_log_file(_OldLog, _NewLog, _FromNode, _ToNode, eof) ->
    ok;
write_changed_log_file(OldLog, NewLog, FromNode, ToNode, {Cont, [LogEntry]}) ->
    ChangedLogEntry = log_change_node(LogEntry, FromNode, ToNode),
    disk_log:log(NewLog, ChangedLogEntry),
    write_changed_log_file(OldLog, NewLog, FromNode, ToNode, disk_log:chunk(OldLog, Cont, 1)).

log_change_node({log_header, _, _, _, FromNode, _} = Log0, FromNode, ToNode) ->
    %% https://github.com/erlang/otp/blob/OTP-20.3.8/lib/mnesia/src/mnesia.hrl#L98
    setelement(5, Log0, ToNode);
log_change_node({commit, FromNode, Decision0, RamCopies, DiscCopies0, DiscOnlyCopies, Ext, SchemaOps0}, FromNode, ToNode) ->
    %% https://github.com/erlang/otp/blob/OTP-20.3.8/lib/mnesia/src/mnesia.hrl#L102
    Decision =
        case Decision0 of
            presume_commit -> Decision0;
            Decision0 -> log_change_node(Decision0, FromNode, ToNode)
        end,
    DiscCopies =
        case DiscCopies0 of
            [FromNode] -> [ToNode];
            Other -> Other
        end,
    SchemaOps = schema_ops_change_node(SchemaOps0, FromNode, ToNode, []),
    {commit, ToNode, Decision, RamCopies, DiscCopies, DiscOnlyCopies, Ext, SchemaOps};
log_change_node({decision, _, _, DiscNodes0, _} = Log0, FromNode, ToNode) ->
    %% https://github.com/erlang/otp/blob/OTP-20.3.8/lib/mnesia/src/mnesia.hrl#L113-L114
    DiscNodes =
        case DiscNodes0 of
            [FromNode] -> [ToNode];
            [] -> [];
            Other -> io:fwrite("Unexpected disc_nodes in decision log entry: ~p~n", [Other])
        end,
    setelement(4, Log0, DiscNodes);
log_change_node(Other, _FromNode, _ToNode) ->
    %% Consider cyclic waits?
    Other.

schema_ops_change_node([], _FromNode, _ToNode, Acc) ->
    lists:reverse(Acc);
schema_ops_change_node([{op, Name, Def} | SchemaOps0], FromNode, ToNode, Acc0) ->
    %% e.g. https://github.com/erlang/otp/blob/OTP-20.3.8/lib/mnesia/src/mnesia_schema.erl#L1593
    Acc = [{op, Name, change_node_in_tab_def(Def, FromNode, ToNode)} | Acc0],
    schema_ops_change_node(SchemaOps0, FromNode, ToNode, Acc);
schema_ops_change_node([{op, Name, Def, Prop} | SchemaOps0], FromNode, ToNode, Acc0) ->
    %% e.g. https://github.com/erlang/otp/blob/OTP-20.3.8/lib/mnesia/src/mnesia_schema.erl#L2196
    Acc = [{op, Name, change_node_in_tab_def(Def, FromNode, ToNode), Prop} | Acc0],
    schema_ops_change_node(SchemaOps0, FromNode, ToNode, Acc);
schema_ops_change_node([Op | Opts], FromNode, ToNode, Acc) ->
    schema_ops_change_node(Opts, FromNode, ToNode, [Op | Acc]).

disk_log_sync(Name) ->
    case disk_log:sync(Name) of
        ok -> ok;
        {error, Reason} ->
            io:fwrite("Syncing ~p log file failed: ~p~n", [Name, Reason]),
            db_renaming_error(Reason)
    end.

disk_log_close(Name) ->
    case disk_log:close(Name) of
        ok -> ok;
        {error, Reason} ->
            io:fwrite("Closing ~p log file failed: ~p~n", [Name, Reason]),
            db_renaming_error(Reason)
    end.

%%%===================================================================
%%% Common
%%%===================================================================

change_node_in_tab_def(TabDef, FromNode, ToNode) ->
    ToBeRenamed =
        lists:filter(
          fun({_Backend, [Node]}) when Node =:= FromNode -> true;
             ({cookie, {_, Node}}) when Node =:= FromNode -> true;
             ({version, {_, {Node, _}}}) when Node =:= FromNode -> true;
             (_Other) -> false
          end, TabDef),
    lists:foldl(
      fun({Backend, [Node]}, Acc) when Node =:= FromNode ->
              lists:keyreplace(Backend, 1, Acc, {Backend, [ToNode]});
         ({cookie, {A, Node}}, Acc) when Node =:= FromNode ->
              lists:keyreplace(cookie, 1, Acc, {cookie, {A, ToNode}});
         ({version, {Vsn, {Node, Ts}}}, Acc) when Node =:= FromNode ->
              lists:keyreplace(version, 1, Acc, {version, {Vsn, {ToNode, Ts}}})
      end, TabDef, ToBeRenamed).

get_nonexisting_file(SchemaDatFile, Suffix) ->
    File = lists:concat([SchemaDatFile, Suffix]),
    case file_exists(File) of
        true -> find_nonexisting_file(SchemaDatFile, Suffix);
        false -> File
    end.

find_nonexisting_file(SchemaDatFile, Suffix) ->
    Ts = erlang:system_time(millisecond),
    File = lists:concat([SchemaDatFile, ".", Ts, Suffix]),
    case file_exists(File) of
        true -> find_nonexisting_file(SchemaDatFile, Suffix);
        false -> File
    end.

file_exists(File) ->
    case file:read_link_info(File) of
        {ok, _} -> true;
        _ -> false
    end.

replace_file(OldFile, NewFile) ->
    case file:delete(OldFile) of
        ok ->
            case file:copy(NewFile, OldFile) of
                {ok, _} ->
                    %% Do not care that much about result of removal of temp log file
                    file:delete(NewFile),
                    ok;
                {error, Reason} ->
                    io:fwrite("Copying file ~p to ~p failed; please fix this manually~n", [OldFile, NewFile]),
                    db_renaming_error(Reason)
            end;
        {error, Reason} ->
            io:fwrite("Removal of file ~p failed: ~p", [OldFile, Reason]),
            db_renaming_error(Reason)
    end.

db_renaming_error(Error) ->
    throw(?db_renaming_error(Error)).
