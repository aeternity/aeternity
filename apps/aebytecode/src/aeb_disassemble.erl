%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Prettyprint aevm machine code
%%% @end
%%% Created : 2 Oct 2017
%%%-------------------------------------------------------------------

-module(aeb_disassemble).

-export([ pp/1,
          format/2,
          format_address/1
        ]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").


pp(Binary) ->
    Listing = format(Binary, fun io:format/2),
    io:format("~s~n", [Listing]).

format(Binary, ErrFormatFun) ->
    pp(0, binary:bin_to_list(Binary), [], ErrFormatFun).

pp(Address, [Op|Ops], Assembly, ErrFormatFun) ->
    case Op of
        X when (X >= ?STOP) andalso (X =< ?SIGNEXTEND) ->
            Instr = pp_instruction(Address, aeb_opcodes:mnemonic(Op), []),
            next(Address, Ops, Instr, Assembly, ErrFormatFun);
        X when (X >= ?LT) andalso (X =< ?BYTE) ->
            Instr = pp_instruction(Address, aeb_opcodes:mnemonic(Op), []),
            next(Address, Ops, Instr, Assembly, ErrFormatFun);
        X when (X >= ?SHA3) andalso (X =< ?SHA3) ->
            Instr = pp_instruction(Address, aeb_opcodes:mnemonic(Op), []),
            next(Address, Ops, Instr, Assembly, ErrFormatFun);
        X when (X >= ?ADDRESS) andalso (X =< ?EXTCODECOPY) ->
            Instr = pp_instruction(Address, aeb_opcodes:mnemonic(Op), []),
            next(Address, Ops, Instr, Assembly, ErrFormatFun);
        X when (X >= ?BLOCKHASH) andalso (X =< ?GASLIMIT) ->
            Instr = pp_instruction(Address, aeb_opcodes:mnemonic(Op), []),
            next(Address, Ops, Instr, Assembly, ErrFormatFun);
        X when (X >= ?POP) andalso (X =< ?JUMPDEST) ->
            Instr = pp_instruction(Address, aeb_opcodes:mnemonic(Op), []),
            next(Address, Ops, Instr, Assembly, ErrFormatFun);
        X when (X >= ?PUSH1) andalso (X =< ?PUSH32) ->
            Bytes = X-?PUSH1+1,
            {ArgList, NextOps} = lists:split(Bytes, Ops),
            Arg = arglist_to_arg(ArgList),
            Instr = pp_instruction(Address, aeb_opcodes:mnemonic(Op), [{Arg,8*Bytes}]),
            next(Address+Bytes, NextOps, Instr, Assembly, ErrFormatFun);
        X when (X >= ?DUP1) andalso (X =< ?LOG4) ->
            Instr = pp_instruction(Address, aeb_opcodes:mnemonic(Op), []),
            next(Address, Ops, Instr, Assembly, ErrFormatFun);
        X when (X >= ?CREATE) andalso (X =< ?DELEGATECALL) ->
            Instr = pp_instruction(Address, aeb_opcodes:mnemonic(Op), []),
            next(Address, Ops, Instr, Assembly, ErrFormatFun);
        X when (X >= ?INVALID) andalso (X =< ?SUICIDE) ->
            Instr = pp_instruction(Address, aeb_opcodes:mnemonic(Op), []),
            next(Address, Ops, Instr, Assembly, ErrFormatFun);
        _ ->
            ErrFormatFun("unhandled op ~p at ~p",[Op, Address]),
            next(Address, Ops, "", Assembly, ErrFormatFun)
    end;
pp(_, [], Assembly, _) -> lists:reverse(Assembly).


arglist_to_arg([B|Bs]) ->
    arglist_to_arg(Bs, B).

arglist_to_arg([B|Bs], Acc) ->
    arglist_to_arg(Bs, Acc*256 + B);
arglist_to_arg([], Acc) -> Acc.

pp_instruction(Address, Op, Args) ->
    [format_address(Address), "   ",
     pad_op(atom_to_list(Op)),
     pp_args(Args),
     "\n"].

format_address(Address) ->
    io_lib:format("0x~8.16.0B",[Address]).

pad_op(Op) ->
    N = length(Op),
    Pad = 17 - N,
    [Op,lists:duplicate(Pad, 32)].

pp_args([]) -> [];
pp_args([{Arg, Size}]) ->
    case Size of
        8 -> io_lib:format("0x~2.16.0B",[Arg]);
        160 -> io_lib:format("0x~64.16.0B",[Arg]);
        232 -> io_lib:format("0x~64.16.0B",[Arg]);
        256 -> io_lib:format("0x~64.16.0B",[Arg]);
        _ -> io_lib:format("0x~64.16.0B",[Arg])
    end;
pp_args([{Arg, Size}|Args]) ->
    [pp_args([{Arg, Size}]), " ", pp_args(Args)].

next(Address, Ops, Instr, Assembly, ErrFormatFun) ->
    pp(Address+1, Ops, [Instr|Assembly], ErrFormatFun).
