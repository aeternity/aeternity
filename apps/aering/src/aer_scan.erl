%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc The Sophia lexer.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aer_scan).

-export([scan/1]).

-import(aer_scan_lib, [token/1, token/2, symbol/0, skip/0,
                       override/2, push/2, pop/1]).

lexer() ->
    DIGIT    = "[0-9]",
    HEXDIGIT = "[0-9a-fA-F]",
    LOWER    = "[a-z_]",
    UPPER    = "[A-Z]",
    CON      = [UPPER, "[a-zA-Z0-9_]*"],
    INT      = [DIGIT, "+"],
    HEX      = ["0x", HEXDIGIT, "+"],
    HASH     = ["#", HEXDIGIT, "+"],
    WS       = "[\\000-\\ ]+",
    ID       = [LOWER, "[a-zA-Z0-9_']*"],
    TVAR     = ["'", ID],
    QID      = ["(", CON, "\\.)+", ID],
    QCON     = ["(", CON, "\\.)+", CON],
    OP       = "[=!<>+\\-*/:&|?~]+",
    CHAR     = "'([^'\\\\]|(\\\\.))'",
    STRING   = "\"([^\"\\\\]|(\\\\.))*\"",

    CommentStart = {"/\\*", push(comment, skip())},
    CommentRules =
        [ CommentStart
        , {"\\*/",        pop(skip())}
        , {"[^/*]+|[/*]", skip()} ],

    Keywords = ["contract", "import", "let", "rec", "switch", "type", "if", "elif", "else", "function",
                "stateful", "true", "false", "and", "mod", "public" "private",
                "band", "bor", "bxor", "bsl", "bsr", "bnot"],
    KW = string:join(Keywords, "|"),

    Rules =
          %% Comments and whitespace
        [ CommentStart
        , {"//.*", skip()}
        , {WS,     skip()}

          %% Special characters
        , {"\\.\\.|[,.;()\\[\\]{}]", symbol()}

          %% Literals
        , {CHAR,   token(char,   fun parse_char/1)}
        , {STRING, token(string, fun parse_string/1)}
        , {HEX,    token(hex,    fun parse_hex/1)}
        , {INT,    token(int,    fun list_to_integer/1)}
        , {HASH,   token(hash,   fun parse_hash/1)}

          %% Identifiers (qualified first!)
        , {QID,   token(qid,  fun(S) -> string:tokens(S, ".") end)}
        , {QCON,  token(qcon, fun(S) -> string:tokens(S, ".") end)}
        , {TVAR,  token(tvar)}
        , override({ID, token(id)}, {KW, symbol()})    %% Keywords override identifiers. Need to
        , {CON, token(con)}                            %% use override to avoid lexing "lettuce"
                                                       %% as ['let', {id, "tuce"}].
          %% Operators
        , {OP, symbol()}
        ],

    [{code, Rules}, {comment, CommentRules}].

scan(String) ->
    Lexer = aer_scan_lib:compile(lexer()),
    aer_scan_lib:string(Lexer, code, String).

%% -- Helpers ----------------------------------------------------------------

parse_string([$" | Chars]) ->
    unescape(Chars).

parse_char([$', $\\, Code, $']) ->
    case Code of
        $'  -> $';
        $\\ -> $\\;
        $b  -> $\b;
        $e  -> $\e;
        $f  -> $\f;
        $n  -> $\n;
        $r  -> $\r;
        $t  -> $\t;
        $v  -> $\v;
        _   -> {error, "Bad control sequence: \\" ++ [Code]}
    end;
parse_char([$', C, $']) -> C.

unescape(Str) -> unescape(Str, []).

%% TODO: numeric escapes
unescape([$"], Acc) ->
    list_to_binary(lists:reverse(Acc));
unescape([$\\, Code | Chars], Acc) ->
    Ok = fun(C) -> unescape(Chars, [C | Acc]) end,
    case Code of
        $"  -> Ok($");
        $\\ -> Ok($\\);
        $b  -> Ok($\b);
        $e  -> Ok($\e);
        $f  -> Ok($\f);
        $n  -> Ok($\n);
        $r  -> Ok($\r);
        $t  -> Ok($\t);
        $v  -> Ok($\v);
        _   -> error("Bad control sequence: \\" ++ [Code])  %% TODO
    end;
unescape([C | Chars], Acc) ->
    unescape(Chars, [C | Acc]).

parse_hex("0x" ++ Chars) -> list_to_integer(Chars, 16).

parse_hash("#" ++ Chars) ->
    N = list_to_integer(Chars, 16),
    <<N:256>>.

