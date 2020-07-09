-module(aec_plugin_xform).

-export([parse_transform/2]).

-ifdef(OTP_RELEASE).

-export([format_error/1]).

parse_transform(Forms, Opts) ->
    Ctxt = parse_trans:initial_context(Forms, Opts),
    Mod = parse_trans:context(module, Ctxt),
    {Pluggable, Forms1} = find_pluggable(Forms),
    case errors(Forms1) of
        [] ->
            case Pluggable of
                [_|_] ->
                    F = fun(Form) -> pt(Form, Pluggable) end,
                    NewForms0 = plain_transform(F, Forms1),
                    LastL = get_pos(lists:last(NewForms0)),
                    append_form(choice_f(Mod, LastL+1), NewForms0);
                [] ->
                    case lists:keymember(error, 1, Forms1) of
                        true ->
                            Forms1;
                        false ->
                            Forms
                    end
            end;
        Errors ->
            {error, Errors, []}
    end.

-else.

parse_transform(Forms, _) ->
    Forms.

-endif.

-ifdef(OTP_RELEASE). %% Support functions

find_pluggable(Forms) ->
    {Blocks, Forms1} = pluggable_blocks(Forms),  % may insert error forms
    Pluggable = try lists:foldl(
                      fun({attribute,_,pluggable,all_exported}, _Acc) ->
                              Exports = all_exports(Forms),
                              throw(#{pluggable => Exports});
                         ({attribute,_,pluggable,Fs}, Acc) ->
                              Fs ++ Acc;
                         (_, Acc) ->
                              Acc
                      end, [], Forms1)
                catch
                    throw:#{pluggable := Ps} -> Ps
                end,
    AllPluggable = lists:flatmap(fun({_, Funs}) -> Funs end, Blocks) ++ Pluggable,
    {AllPluggable, Forms1}.

pluggable_blocks(Forms) ->
    #{acc := Acc, blocks := Blocks} = pluggable_blocks(Forms, #{acc => [], blocks => []}),
    {Blocks, Acc}.
    

pluggable_blocks([{attribute,_,file,{File,_}} = F|Fs], #{acc := Acc0} = Acc) ->
    pluggable_blocks(Fs, Acc#{file => File, acc => [F | Acc0]});
pluggable_blocks([{attribute,L,pluggable_begin,Tag} = F|Fs],
                 #{acc := Acc0} = Acc) ->
    case maps:find(current_block, Acc) of
        {ok, Prev} ->
            File = maps:get(file, Acc),
            Err = {error, {File, L}, {overlapping_pluggable_blocks, [Prev, Tag]}},
            pluggable_blocks(Fs, Acc#{acc => [Err, F | Acc0]});
        error ->
            pluggable_blocks(Fs, Acc#{current_block => Tag,
                                      acc => [F | Acc0]})
    end;
pluggable_blocks([{attribute,_,pluggable_end,Tag} = F|Fs],
                 #{current_block := Tag,
                   blocks := Blocks,
                   acc := Acc0} = Acc) ->
    Exports = maps:get(exports, Acc, []),
    Acc1 = maps:remove(current_block, Acc),
    pluggable_blocks(Fs, Acc1#{acc => [F | Acc0],
                               blocks => [{Tag, Exports}|Blocks]});
pluggable_blocks([{attribute,_,export,Funs} = F|Fs],
                 #{ current_block := _
                  , acc := Acc0} = Acc) ->
    Exports = maps:get(exports, Acc, []),
    pluggable_blocks(Fs, Acc#{exports => Exports ++ Funs, acc => [F|Acc0]});
pluggable_blocks([{eof,L} = F], #{current_block := Tag, acc := Acc0} = Acc) ->
    File = maps:get(file, Acc),
    Acc#{acc => lists:reverse(
                  [F,
                   {error, {File, L}, {unterminated_pluggable_block, [Tag]}}
                   | Acc0])};
pluggable_blocks([F|Fs], #{acc := Acc0} = Acc) ->
    pluggable_blocks(Fs, Acc#{acc := [F|Acc0]});
pluggable_blocks([], #{acc := Acc0} = Acc) ->
    Acc#{acc => lists:reverse(Acc0)}.

all_exports(Forms) ->
    case [1 || {attribute,_,compile,export_all} <- Forms] of
        [] ->
            all_explicit_exports(Forms);
        [_|_] ->
            all_functions(Forms)
    end.

all_explicit_exports(Forms) ->
    lists:flatten([Fs || {attribute,_,export,Fs} <- Forms]).

all_functions(Forms) ->
    [{F,A} || {function,_,F,A,_} <- Forms].

pt({function,L,Fun,Arity,Clauses} = Form, Pluggable) ->
    case lists:member({Fun,Arity}, Pluggable) of
        true ->
            Args0 = args(Arity, L),
            %% F0 = list_to_atom(atom_to_list(Fun) ++ "___i"),
            Clauses1 = [{clause, L, Args0, [],
                         [{'case',L,
                           {call,L,{atom,L,'--is_plugged--'},
                            [{atom,L,Fun}, {integer,L,Arity}]},
                           [{clause,L,[{atom,L,false}], [],
                             [{'case',L,{tuple,L,Args0},
                               [{clause,L1,[{tuple,L1,As}],Gs,B}
                                || {clause,L1,As,Gs,B} <- Clauses]}
                             ]},
                            {clause,L,[{tuple,L,[{atom,L,true},
                                                 {var,L,'__M__'}]}],
                             [],
                             [{call,L,{remote,L,{var,L,'__M__'},{atom,L,Fun}},
                               Args0}]}]}]
                        }],
            {function,L,Fun,Arity,Clauses1};
             %% {function,L,F0, Arity,Clauses}];
        false ->
            Form
    end;
pt(Form, _) ->
    Form.

args(N, L) ->
    [{var,L,v(X)} || X <- lists:seq(1,N)].

v(X) ->
    list_to_atom("$V_" ++ integer_to_list(X)).

choice_f(Mod, L) ->
    {function,L,'--is_plugged--',2,
     [{clause,L,
       [{var,L,'Fun'},{var,L,'Arity'}],
       [],
       [{'case',L,
         {call,L,
          {remote,L,{atom,L,aec_plugin},{atom,L,get_module}},
          [{atom,L,Mod}]},
         [{clause,L,[{atom,L,undefined}],[],[{atom,L,false}]},
          {clause,L,
           [{var,L,'NewMod'}],
           [],
           [{'case',L,
             {call,L,{remote,L,{atom,L,erlang},{atom,L,function_exported}},
              [{var,L,'NewMod'},{var,L,'Fun'},{var,L,'Arity'}]},
             [{clause,L,[{atom,L,true}],[],
               [{tuple,L,[{atom,L,true},{var,L,'NewMod'}]}]},
              {clause,L,[{atom,L,false}],[],
               [{atom,L,false}]}
              ]}
            ]}
          ]}
        ]}
     ]}.

%% Modified version of parse_trans:plain_transform/2
%%
plain_transform(Fun, Forms) when is_function(Fun, 1), is_list(Forms) ->
    plain_transform1(Fun, Forms).

plain_transform1(_, []) ->
    [];
plain_transform1(Fun, [F|Fs]) when element(1,F) == error ->
    [F|plain_transform1(Fun, Fs)];
plain_transform1(Fun, [F|Fs]) when is_atom(element(1,F)) ->
    case Fun(F) of
        skip ->
            plain_transform1(Fun, Fs);
        continue ->
            [list_to_tuple(plain_transform1(Fun, tuple_to_list(F))) |
             plain_transform1(Fun, Fs)];
        {done, NewF} ->
            [NewF | Fs];
        NewF when is_tuple(NewF) ->
            %% includes error tuples
            [NewF | plain_transform1(Fun, Fs)];
        NewFs when is_list(NewFs) ->
            NewFs ++ plain_transform1(Fun, Fs)
    end;
plain_transform1(Fun, [L|Fs]) when is_list(L) ->
    [plain_transform1(Fun, L) | plain_transform1(Fun, Fs)];
plain_transform1(Fun, [F|Fs]) ->
    [F | plain_transform1(Fun, Fs)];
plain_transform1(_, F) ->
    F.

get_pos(F) when is_list(F) ->
    parse_trans:get_pos(F);
get_pos(Form) ->
    element(2, Form).

append_form(F, Forms) ->
    [{eof,_}|RevForms] = lists:reverse(Forms),
    NewLastPos = get_pos(F),
    lists:reverse(RevForms) ++ [F,{eof,NewLastPos+1}].

errors(Forms) ->
    case [{F, {L, ?MODULE, E}} || {error, {F, L}, E} <- Forms] of
        [] ->
            [];
        Errs ->
            lists:foldl(fun append_err/2, orddict:new(), Errs)
    end.

append_err({F, E}, Acc) ->
    orddict:append(F, E, Acc).

format_error({overlapping_pluggable_blocks, Bs}) ->
    io_lib:format("Overlapping 'pluggable' blocks: ~p", [Bs]);
format_error({unterminated_pluggable_block, Tag}) ->
    io_lib:format("Unterminated 'pluggable' block: ~p", [Tag]);
format_error(Err) ->
    io_lib:fwrite("~w", [Err]).

-endif.
