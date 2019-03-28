-module(aefa_gen_dispatch).
%% Helper to generate code for eval dispatch.
%% Could be extended with fetching and writing arguments
%% and checking types in the future.
%% When new instructions have been added or changend in the
%% Fate op specification in aebytecode do:
%% $ make console
%% 1> aefa_gen_dispatch:gen_dispatch("aefa_fate_eval.template").
%% Put the template code in aefa_fate_eval.erl at the right place.
%%
-export([gen_dispatch/1]).

gen_dispatch(Filename) ->
    {ok, File} = file:open(Filename, [write]),
    Ops = aeb_fate_generate_ops:get_ops(),
    Instructions = lists:flatten([gen_eval(Op)++"\n" || Op <- Ops]),
    io:format(File,
              "~s"
              "eval(Op, EngineState) ->\n"
              "    throw({error, unknown_op, Op}).\n"
             , [Instructions]),
    io:format(File, "\n", []),
    file:close(File).

gen_eval(#{ opname            := Name
          , end_bb            := true
          , format            := atomic
          , constructor       := Constructor
          }) ->
    io_lib:format(
      "eval(~w, EngineState) ->\n"
      "    aefa_fate_op:~w(EngineState);\n",
      [Name, Constructor]);
gen_eval(#{ opname            := Name
          , format            := atomic
          , constructor       := Constructor
          }) ->
    io_lib:format(
      "eval(~w, EngineState) ->\n"
      "    {next, aefa_fate_op:~w(EngineState)};\n",
      [Name, Constructor]);
gen_eval(#{ opname            := Name
          , opcode            :=_OpCode
          , args              := Arity
          , end_bb            := true
          , format            := FateFormat
          , macro             :=_Macro
          , type_name         :=_TypeName
          , doc               :=_Doc
          , gas               :=_Gas
          , type              :=_Type
          , constructor       := Constructor
          , constructor_type  :=_ConstructorType
          }) ->
    Args  = gen_arg_matches(FateFormat, 0),
    CArgs = [io_lib:format("Arg~w, ", [N]) || N <- lists:seq(0, Arity-1)],
    io_lib:format(
      "eval({~w ~s}, EngineState) ->\n"
      "    aefa_fate_op:~w(~sEngineState);\n",
      [Name, Args, Constructor, CArgs]);
gen_eval(#{ opname            := Name
          , opcode            :=_OpCode
          , args              := Arity
          , end_bb            := false
          , format            := FateFormat
          , macro             :=_Macro
          , type_name         :=_TypeName
          , doc               :=_Doc
          , gas               :=_Gas
          , type              :=_Type
          , constructor       := Constructor
          , constructor_type  :=_ConstructorType
          }) ->
    Args  = gen_arg_matches(FateFormat, 0),
    CArgs = [io_lib:format("Arg~w, ", [N]) || N <- lists:seq(0, Arity-1)],
    io_lib:format(
      "eval({~w ~s}, EngineState) ->\n"
      "    {next, aefa_fate_op:~w(~sEngineState)};\n",
      [Name, Args, Constructor, CArgs]).



gen_arg_matches([], _) ->
    "";
gen_arg_matches([ a | Rest], N) ->
    io_lib:format(", Arg~w", [N]) ++ gen_arg_matches(Rest, N+1);
gen_arg_matches([is | Rest], N) ->
    io_lib:format(", {immediate, Arg~w}", [N]) ++ gen_arg_matches(Rest, N+1);
gen_arg_matches([li | Rest], N) ->
    io_lib:format(", {immediate, Arg~w}", [N]) ++ gen_arg_matches(Rest, N+1);
gen_arg_matches([ii | Rest], N) ->
    io_lib:format(", {immediate, Arg~w}", [N]) ++ gen_arg_matches(Rest, N+1).


