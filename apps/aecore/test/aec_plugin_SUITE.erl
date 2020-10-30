-module(aec_plugin_SUITE).

-export([
          all/0
        , groups/0
        , suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_group/2
        , end_per_group/2
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% test case exports

-export([
          registration/1
        , compile_unpluggable/1
        , compile_pluggable_export_all/1
        , compile_pluggable_begin_end/1
        , compile_pluggable_begin_no_end/1
        , compile_pluggable_overlap/1
        ]).

-define(TMOD, plugin_test_module).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, all_tests}].

groups() ->
    [
      {all_tests, [sequence], [ {group, plugin}
                              , {group, compile}]}
    , {plugin,
       [ registration
       ]}
    , {compile,
       [ compile_unpluggable
       , compile_pluggable_export_all
       , compile_pluggable_begin_end
       , compile_pluggable_begin_no_end
       , compile_pluggable_overlap
       ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(compile, Config) ->
    case system_major_vsn() >= 21 of
        true ->
            Config;
        false ->
            {skip, not_supported_before_OTP21}
    end;
init_per_group(_Grp, Config) ->
    Config.

end_per_group(_Grp, _Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%%%===================================================================


registration(_Cfg) ->
    case system_major_vsn() >= 21 of
        true ->
            real_registration();
        false ->
            legacy_registration()
    end,
    ok.

real_registration() ->
    undefined = aec_plugin:get_module(aec_tx_pool),
    ok = aec_plugin:register(#{aec_tx_pool => ?MODULE}),
    ?MODULE = aec_plugin:get_module(aec_tx_pool),
    ok.

legacy_registration() ->
    undefined = aec_plugin:get_module(aec_tx_pool),
    try begin
            aec_plugin:register(#{aec_tx_pool => ?MODULE}),
            error(should_fail)
        end
    catch
        error:requires_OTP21 ->
            undefined = aec_plugin:get_module(aec_tx_pool),
            ok
    end.

compile_unpluggable(_Cfg) ->
    compile_tmod(plain).

compile_pluggable_export_all(_Cfg) ->
    compile_tmod(pluggable_export_all).

compile_pluggable_begin_end(_Cfg) ->
    compile_tmod(pluggable_begin_end).

compile_pluggable_begin_no_end(_Cfg) ->
    compile_tmod(pluggable_begin_no_end, error).

compile_pluggable_overlap(_Cfg) ->
    compile_tmod(pluggable_overlap, error).

compile_tmod(Tag) ->
    compile_tmod(Tag, ok).

compile_tmod(Tag, Expect) ->
    {Fs, Forms} = forms(Tag),
    case compile_tmod(Tag, Forms, Expect) of
        {ok, Abst, _} ->
            true = are_pluggable(Fs, Abst),
            ok;
        expected_error ->
            ok
    end.

compile_tmod(Tag, Forms, Expect) ->
    Src = [erl_pp:form(F) || F <- Forms],
    case {compile:forms(Forms, [{parse_transform, aec_plugin_xform},
                                return_errors, return_warnings, binary, debug_info]),
          Expect} of
        {{ok, ?TMOD, Bin, []}, ok} ->
            {ok,{?TMOD,[{abstract_code,{raw_abstract_v1,Abst}}]}} =
                beam_lib:chunks(Bin, [abstract_code]),
            ct:log("Abst = ~p", [Abst]),
            {ok, Abst, Bin};
        {{error, Errors, Warnings}, _} ->
            ct:log("Errors returned: ~p~n(Warnings = ~p)", [Errors, Warnings]),
            list_errors(Errors),
            case Expect of
                ok ->
                    error(errors);
                error ->
                    expected_error
            end;
        Other ->
            ct:log("Other = ~p~n"
                   "Tag   = ~p~n"
                   "Forms = ~p~n"
                   "Src:~n~s", [Other, Tag, Forms, Src]),
            error(compile_error_or_warning)
    end.

system_major_vsn() ->
    try list_to_integer(erlang:system_info(otp_release))
    catch
        error:_ ->
            0
    end.

forms(pluggable_export_all = T) ->
    Forms0 = plain_forms(T),
    Pred = fun({attribute,_,export,_}) -> true end,
    Forms = insert_forms(Forms0, Pred,
                         [{attribute,1,pluggable,all_exported}], []),
    ct:log("Forms0 = ~p~n"
           "Forms  = ~p", [Forms0, Forms]),
    {[{f1,0},{f2,1}], Forms};
forms(pluggable_begin_end = T) ->
    Forms0 = plain_forms(T),
    Pred = fun({attribute,_,export,[{f1,0}]}) -> true end,
    Forms = insert_forms(Forms0, Pred,
                         [{attribute,1,pluggable_begin,f1}],
                         [{attribute,1,pluggable_end  ,f1}]),
    ct:log("Forms0 = ~p~n"
           "Forms  = ~p", [Forms0, Forms]),
    {[{f1,0}], Forms};
forms(pluggable_begin_no_end = T) ->
    Forms0 = plain_forms(T),
    Pred = fun({attribute,_,export,[{f1,0}]}) -> true end,
    Forms = insert_forms(Forms0, Pred,
                         [{attribute,1,pluggable_begin, f1}],
                         []),
    {[], Forms};
forms(pluggable_overlap = T) ->
    Forms0 = plain_forms(T),
    Pred = fun({attribute,_,export,[{f1,0}]}) -> true end,
    Forms = insert_forms(Forms0, Pred,
                         [{attribute,1,pluggable_begin, f1}],
                         [{attribute,1,pluggable_begin, f2},
                          {attribute,1,pluggable_end  , f1}]),
    Pred1 = fun({attribute,_,export,[{f2,0}]}) -> true end,
    Forms1 = insert_forms(Forms, Pred1,
                          [],
                          [{attribute,1,pluggable_end, f2}]),
    {[], Forms1};
forms(plain) ->
    {[], plain_forms(plain)}.

plain_forms(Tag) when is_atom(Tag) ->
    [ {attribute,1,file,{erl_fname(?TMOD),1}}
    , {attribute,2,module,?TMOD}
    , {attribute,3,export,[{f1,0}]}
    , {attribute,4,export,[{f2,1}]}
    , {function,5,f1,0,
       [{clause,5,[],[],[{tuple,5,[{atom,5,?TMOD},
                                   {atom,5,Tag},
                                   {atom,5,f1}]}]}]}
    , {function,6,f2,1,
       [{clause,6,[{var,6,'_'}],[],[{tuple,6,[{atom,6,?TMOD},
                                              {atom,6,Tag},
                                              {atom,6,f1}]}]}]}
    , {eof,7}
    ].

erl_fname(Mod) ->
    atom_to_list(Mod) ++ ".erl".

insert_forms([F|Fs] = Forms, Pred, Bef, Aft) ->
    case pred(Pred, F) of
        True when True == true; element(1, True) == true ->
            L = line(F),
            N = get_n(True),
            {Between, Tail} = lists:split(N, Forms),
            {Bef1, L1} = renumber(Bef, L),
            {Between1, L2} = renumber(Between, L1+1),
            {Aft1, L3} = renumber(Aft, L2+1),
            {Tail1 , _ } = renumber(Tail, L3+1),
            Bef1 ++ Between1 ++ Aft1 ++ Tail1;
        false ->
            [F | insert_forms(Fs, Pred, Bef, Aft)]
    end;
insert_forms([], _, _, _) ->
    [].

pred(P, F) ->
    try P(F)
    catch
        error:_ ->
            false
    end.

line(F) when is_tuple(F) ->
    element(2, F).

get_n(true) ->
    1;
get_n({true, N}) when is_integer(N), N > 0 ->
    N.

renumber(Fs, L) when is_list(Fs) ->
    lists:mapfoldl(fun set_line/2, L, Fs).

set_line({attribute,_,file,{F,_}}, L) ->
    {{attribute,L,file,{F,L}}, L+1};
set_line({attribute,_,A,Fs}, L) ->
    {{attribute,L,A,Fs}, L+1};
set_line({function,_,F,A,Cs}, L) ->
    {Cs1, L1} = renumber_exprs(Cs, L),
    {{function,L,F,A,Cs1}, L1};
set_line(F, L) when tuple_size(F) >= 2 ->
    {setelement(2, F, L), L+1};
set_line(F, L) ->
    {F, L}.

renumber_exprs(Es, L) ->
    renumber_exprs(Es, L, []).

renumber_exprs([E|Es], L, Acc) ->
    {E1, L1, Le} =
        if is_list(E) ->
                {Ex, Lx} =
                    lists:mapfoldl(fun renumber_expr_/2, L, E),
                {Ex, Lx, next_l(E, L)};
           is_tuple(E) ->
                {E1x, L1x} = set_line(E, L),
                {E2x, L2x} = renumber_expr_t(E1x, L1x),
                {E2x, L2x, line(E)};
           true ->
                {E, L, L}
        end,
    NextL = case next_l(Es, Le) of
                Le -> L1;
                Ln -> L1+1 + (Ln-Le)
            end,
    renumber_exprs(Es, NextL, [E1|Acc]);
renumber_exprs([], L, Acc) ->
    {lists:reverse(Acc), L}.

renumber_expr_(E, L) ->
    if is_list(E) ->
            renumber_exprs(E, L);
       is_tuple(E) ->
            {E1x, L1x} = set_line(E, L),
            renumber_expr_t(E1x, L1x);
       true ->
            {E, L}
    end.

next_l([F|_], _) when is_tuple(F) ->
    line(F);
next_l([Fs|_], Def) when is_list(Fs) ->
    next_l(Fs, Def);
next_l(_, Def) ->
    Def.

renumber_expr_t(E, L) when is_tuple(E) ->
    {E1, L1} =
        renumber_exprs(tuple_to_list(E), L),
    {list_to_tuple(E1), L1}.

list_errors(Errs) ->
    lists:foreach(
      fun({F, Es}) ->
              list_errors(F, Es)
      end, Errs).

%% Copied (modified) from compile.erl
%% list_errors(File, ErrorDescriptors) -> ok

list_errors(F, [{none,Mod,E}|Es]) ->
    ct:log("~ts: ~ts\n", [F,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{{Line,Column},Mod,E}|Es]) ->
    ct:log("~ts:~w:~w: ~ts\n", [F,Line,Column,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{Line,Mod,E}|Es]) ->
    ct:log("~ts:~w: ~ts\n", [F,Line,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{Mod,E}|Es]) ->
    %% Not documented and not expected to be used any more, but
    %% keep a while just in case.
    ct:log("~ts: ~ts\n", [F,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(_F, []) -> ok.

are_pluggable(Fs, Forms) ->
    lists:all(
      fun({F,A}) ->
              case get_function_clauses(F, A, Forms) of
                  error ->
                      ct:log("Cannot find ~p", [{F,A}]),
                      false;
                  Cs ->
                      is_pluggable(Cs)
              end
      end, Fs).

get_function_clauses(F,A, Forms) ->
    case [Cs || {function,_,Fx,Ax,Cs} <- Forms,
                Fx == F, Ax == A] of
        [Found] ->
            Found;
        [] ->
            error
    end.

is_pluggable({call,_,{atom,_,'--is_plugged--'},_}) ->
    true;
is_pluggable(Cs) when is_list(Cs) ->
    lists:any(fun is_pluggable/1, Cs);
is_pluggable(C) when is_tuple(C) ->
    is_pluggable(tuple_to_list(C));
is_pluggable(_) ->
    false.
                       
