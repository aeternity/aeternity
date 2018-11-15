-module(aeso_ast).

-export([int/2,
         line/1,
         pp/1,
         pp_typed/1,
         symbol/2,
         symbol_name/1
        ]).


symbol(Line, Chars) -> {symbol, Line, Chars}.
int(Line, Int) -> {'Int', Line, Int}.

line({symbol, Line, _}) -> Line.

symbol_name({symbol, _, Name}) -> Name.

pp(Ast) ->
    %% TODO: Actually do *Pretty* printing.
    io:format("~p~n", [Ast]).

pp_typed(TypedAst) ->
    String = prettypr:format(aeso_pretty:decls(TypedAst, [show_generated])),
    %%io:format("Typed tree:\n~p\n",[TypedAst]),
    io:format("Type info:\n~s\n",[String]).

