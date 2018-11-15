-module(aeso_constants).

-export([string/1, get_type/1]).

string(Str) ->
    case aeso_parser:string("let _ = " ++ Str) of
        {ok, [{letval, _, _, _, E}]} -> {ok, E};
        {ok, Other}                  -> error({internal_error, should_be_letval, Other});
        Err                          -> Err
    end.

get_type(Str) ->
    case aeso_parser:string("let _ = " ++ Str) of
        {ok, [Ast]} ->
            AstT = aeso_ast_infer_types:infer_constant(Ast),
            T = ast_to_type(AstT),
            {ok, T};
        {ok, Other} -> error({internal_error, should_be_letval, Other});
        Err         -> Err
    end.

ast_to_type({id, _, T}) ->
    T;
ast_to_type({tuple_t, _, []}) -> "()";
ast_to_type({tuple_t, _, Ts}) ->
    "(" ++ list_ast_to_type(Ts) ++ ")";
ast_to_type({app_t,_, {id, _, "list"}, [T]}) ->
    lists:flatten("list(" ++ ast_to_type(T) ++ ")");
ast_to_type({app_t,_, {id, _, "option"}, [T]}) ->
    lists:flatten("option(" ++ ast_to_type(T) ++ ")").

list_ast_to_type([T]) ->
    ast_to_type(T);
list_ast_to_type([T|Ts]) ->
    ast_to_type(T)
        ++ ", "
        ++ list_ast_to_type(Ts).





