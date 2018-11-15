-module(ct_eunit_xform).

-export([parse_transform/2]).


parse_transform(Forms, Opts) ->
    Ctxt = parse_trans:initial_context(Forms, Opts),
    [Mod] = [M || {attribute,_,module,M} <- Forms],
    F = atom_to_list(Mod) ++ ".erl",
    [Fullname|_] = [Fn || {attribute,_,file,{Fn,_}} <- Forms,
                          filename:basename(Fn) == F],
    Dir = filename:dirname(Fullname),
    AllMods = all_mods(Dir),
    NewForms =
        parse_trans:plain_transform(fun(Form) ->
                                            xform(Form, AllMods)
                                    end, Forms),
    Res = parse_trans:do_insert_forms(
            above,
            [{attribute,?LINE,pt_pp_src,true},
             {attribute,?LINE,export,
              [{M,1} || {M,_} <- AllMods]}],
            parse_trans:do_insert_forms(
              below,
              add_functions(AllMods),
              NewForms,
              Ctxt),
            Ctxt),
    parse_trans:optionally_pretty_print(Res, Opts, Ctxt),
    Res.

xform({function,_,groups,0,_}, AllMods) ->
    {done, {function,?LINE,groups,0,
            [{clause, ?LINE, [], [],
              [erl_parse:abstract(
                 [{eunit,
                   [M || {M, _Fs} <- AllMods]}], [{line, ?LINE}])]}
            ]}
    };
xform(F, _) ->
    F.

add_functions(AllMods) ->
    [{function,?LINE,M,1,
      [{clause,?LINE,[{var,?LINE,'_Config'}],[],
        [{match, ?LINE,
          {atom, ?LINE, ok},
          {call,?LINE,{remote,?LINE,{atom,?LINE,eunit},{atom,?LINE,test}},
          [{atom,?LINE,M}]}}]
       }]
     } || {M, _} <- AllMods].

all_mods(Dir) ->
  filelib:fold_files(
    Dir, ".*_tests\\.beam", false,
    fun(F, Acc) ->
            case get_eunit_tests(F) of
                {ok, Mod, Tests} ->
                    [{Mod, Tests}|Acc];
                error ->
                    Acc
            end
    end, []).

get_eunit_tests(F) ->
    case beam_lib:chunks(F, [imports, exports]) of
        {ok, {Mod, Chunks}} ->
            case lists:member({eunit, test, 1},
                              proplists:get_value(imports, Chunks, [])) of
                true ->
                    Exports = proplists:get_value(exports, Chunks, []),
                    case [F1 || {F1, 0} <- Exports,
                                is_test_f(F1)] of
                        [] ->
                            error;
                        [_|_] = Fs ->
                            {ok, Mod, Fs}
                    end;
                false ->
                    error
            end;
        _ ->
            error
    end.

is_test_f(F) ->
    case re:run(atom_to_list(F), ".+_test_?$", []) of
        {match,_} ->
            true;
        nomatch ->
            false
    end.
