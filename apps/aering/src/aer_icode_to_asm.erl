%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Translator from Aering Icode to Aevm Assebly
%%% @end
%%% Created : 21 Dec 2017
%%% 
%%%-------------------------------------------------------------------
-module(aer_icode_to_asm).

-export([convert/2]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aer_icode.hrl").

convert(#{ contract_name := Name
         , functions := Functions
              },
        Options) ->
    {_Env, Code} =
        assemble(Functions,  #{ options => Options
                              , vars => []
                              , functions => []
                              , sp => 0
                              , frame_size => 0
                              , address => 0
                              },
                 [aeb_opcodes:mnemonic(?COMMENT), "CONTRACT: " ++ Name]),
    Code.

assemble([{Name, Args, Body}|More], #{frame_size := FS
                                     , address := Address} = Env, Code) ->
    Env0 = add_function(Name, Address, Env),
    {BodyCode, NewEnv} = assemble_body(Body, add_args(Args, Env0, FS), Code),
    assemble(More, NewEnv, BodyCode);
assemble([], Env, Code) -> {Env, Code}.


assemble_body({var_ref, Id}, #{sp := SP} = Env, Code) ->
    SL = lookup_var(Id, Env),
    Instr = aeb_opcodes:mnemonic(?DUP1 + SP + SL),
    NewEnv = inc_sp(Env),
    {Code ++ [Instr], NewEnv}.

inc_sp( #{sp := SP} = Env ) ->  Env#{ sp => SP + 1}.

add_args([{Id, Type}| Rest], #{ vars := Args } = Env, ArgPos) ->
    add_args(Rest,
             Env#{ vars => [{Id, Type, ArgPos} | Args] },
             ArgPos + 1);
add_args([], Env, FS) -> Env#{ frame_size => FS}.

lookup_var(ID, #{ vars := Vars}) ->
    var_sp(ID, Vars).

var_sp(ID, [{ID,_Type, SP} | _]) ->  SP;
var_sp(ID, [_|Rest] ) -> var_sp(ID, Rest);
var_sp(ID, []) -> error({var_out_of_scope, ID}).

          
add_function(Name, Address, #{ functions := Functions } = Env) ->
    Env#{ functions => [{Name, Address} | Functions]}.
