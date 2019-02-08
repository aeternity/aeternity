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
%% TODO: Maybe parametrize FROM_NAME & TO_NAME.
%% This may be easier achieved with dets:select/3 instead of dets:traverse/2?
-define(FROM_NAME, 'epoch@localhost').
-define(TO_NAME, 'aeternity@localhost').
-define(db_renaming_error(E), {db_renaming_error, E}).

%%%===================================================================
%%% API
%%%===================================================================

change_node(SchemaDATFilePath) ->
    %% For now it is hardcoded to change node
    %% from epoch@localhost to aeternity@localhost.
    try do_change_node(SchemaDATFilePath)
    catch throw:?db_renaming_error(Error) -> {error, Error}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_change_node(SchemaDatFile) ->
    ok  = assert_file_present(SchemaDatFile),
    Ref = dets_open_file(SchemaDatFile),
    ok  = dets_change_node(Ref),
    ok  = dets_close_file(Ref).

assert_file_present(SchemaDatFile) ->
    case filelib:is_regular(SchemaDatFile) of
        true -> ok;
        false ->
            io:fwrite("Schema file not found: ~p~n", [SchemaDatFile]),
            db_renaming_error(file_not_found)
    end.

dets_open_file(SchemaDatFile) ->
    case dets:open_file(SchemaDatFile) of
        {ok, Ref} -> Ref;
        {error, Reason} ->
            io:fwrite("Opening SCHEMA.DAT file failed: ~p~n", [Reason]),
            db_renaming_error(Reason)
    end.

dets_change_node(Ref) ->
    case dets:traverse(Ref, fun rename_node/1) of
        {error, Reason} ->
            io:fwrite("Traversing SCHEMA.DAT file failed: ~p~n", [Reason]),
            db_renaming_error(Reason);
        UpdatedSchemaEntries ->
            case dets:insert(Ref, UpdatedSchemaEntries) of
                ok -> ok;
                {error, Reason} ->
                    io:fwrite("Inserting entries into SCHEMA.DAT failed: ~p~n", [Reason]),
                    db_renaming_error(Reason)
            end
    end.

rename_node({schema, Name, Data}) ->
    ToBeRenamed =
        lists:filter(
          fun({_Backend, [?FROM_NAME]}) -> true;
             ({cookie, {_, ?FROM_NAME}}) -> true;
             ({version, {_, {?FROM_NAME, _}}}) -> true;
             (_Other) -> false
          end, Data),
    NewData =
        lists:foldl(
          fun({Backend, [?FROM_NAME]}, Acc) ->
                  lists:keyreplace(Backend, 1, Acc, {Backend, [?TO_NAME]});
             ({cookie, {A, ?FROM_NAME}}, Acc) ->
                  lists:keyreplace(cookie, 1, Acc, {cookie, {A, ?TO_NAME}});
             ({version, {Vsn, {?FROM_NAME, Ts}}}, Acc) ->
                  lists:keyreplace(version, 1, Acc, {version, {Vsn, {?TO_NAME, Ts}}})
          end, Data, ToBeRenamed),
    {continue, {schema, Name, NewData}}.

dets_close_file(Ref) ->
    case dets:close(Ref) of
        ok -> ok;
        {error, Reason} ->
            io:fwrite("Closing SCHEMA.DAT file failed: ~p~n", [Reason]),
            db_renaming_error(Reason)
    end.

db_renaming_error(Error) ->
    throw(?db_renaming_error(Error)).
