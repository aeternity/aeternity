%% This contains a test for me to run on my laptop--John
-module(setup).
-compile(export_all).

setup() ->
  code:add_patha("../../aebytecode/ebin"),
  code:add_patha("../../aevm/ebin").

dummy_state(Code,Data) ->
  #{ gas        => 10000,
     code       => Code,
     cp         => 0,
     memory     => #{},
     stack      => [],
     do_trace   => false,    %% set to true for step-by-step tracing
     trace      => [],
     trace_fun  => fun io:format/2,
     data       => Data
  }.

test(Call) ->
  Code = aer_compiler:file(test,[pp_ast,pp_icode]),
  io:format("\nCompiled code:\n"),
  io:format("~p\n\n",[Code]),
  ok = aeb_disassemble:pp(Code),
  %% Load the call
  {0,Data} = aer_data:to_binary(Call),
  io:format("Running:\n"),
  State = aevm_eeevm:eval(dummy_state(Code, Data)),
  %%io:format("\nFinal state:\n~p\n",[State]),
  io:format("\nFinal stack: ~p\n",[maps:get(stack,State)++[end_of_stack]]),
  ok.

