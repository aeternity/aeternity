-module(aer_ast).

-export([int/2,
         line/1,
         symbol/2,
         symbol_name/1
        ]).

-include("aer_ast.hrl").

symbol(Line, Chars) -> {symbol, Line, Chars}.
int(Line, Int) -> {'Int', Line, Int}.

line({symbol, Line, _}) -> Line.

symbol_name({symbol, _, Name}) -> Name.

