-module(aec_plugin_xform).

-export([parse_transform/2]).


parse_transform(Forms, Opts) ->
    Ctxt = parse_trans:initial_context(Forms, Opts),
    Mod = parse_trans:context(module, Ctxt),
    Pluggable = find_pluggable(Forms),
    F = fun(Form) -> pt(Form, Pluggable) end,
    NewForms0 = plain_transform(F, Forms),
    LastL = get_pos(lists:last(NewForms0)),
    NewForms = append_form(choice_f(Mod, LastL+1), NewForms0),
    NewForms.

find_pluggable(Forms) ->
    lists:foldl(
      fun({attribute,_,pluggable,Fs}, Acc) ->
              Fs ++ Acc;
         (_, Acc) ->
              Acc
      end, [], Forms).

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
