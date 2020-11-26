
-record(proxy_mp_tree, {
                         mod     :: module()
                       , state   :: term()
                       }).

-record(proxy_mp_tree_iter, {
                              mod      :: module()
                            , iterator :: aeu_mp_trees:iterator()
                            }).

%% Operations that only return value
%%
-define(PROXY_GET(F, Mod, P),
        F(#proxy_mp_tree{mod = Mod} = P) -> Mod:proxy_get(F, ?MODULE, {}, P)).
                
-define(PROXY_GET(F, Arg1, Mod, P),
        F(Arg1, #proxy_mp_tree{mod = Mod} = P) ->
               Mod:proxy_get(F, ?MODULE, {Arg1}, P)).

-define(PROXY_GET(F, Arg1, Arg2, Mod, P),
        F(Arg1, Arg2, #proxy_mp_tree{mod = Mod} = P) ->
               Mod:proxy_get(F, ?MODULE, {Arg1, Arg2}, P)).

-define(PROXY_GET(F, Arg1, Arg2, Arg3, Mod, P),
        F(Arg1, Arg2, #proxy_mp_tree{mod = Mod} = P) ->
               Mod:proxy_get(F, ?MODULE, {Arg1, Arg2, Arg3}, P)).

%% Operations that return {Result, NewTree}
%%
-define(PROXY_PUT(F, Mod, P),
        F(#proxy_mp_tree{mod = Mod} = P) -> Mod:proxy_put(F, ?MODULE, {}, P)).
                
-define(PROXY_PUT(F, Arg1, Mod, P),
        F(Arg1, #proxy_mp_tree{mod = Mod} = P) ->
               Mod:proxy_put(F, ?MODULE, {Arg1}, P)).

-define(PROXY_PUT(F, Arg1, Arg2, Mod, P),
        F(Arg1, Arg2, #proxy_mp_tree{mod = Mod} = P) ->
               Mod:proxy_put(F, ?MODULE, {Arg1, Arg2}, P)).

-define(PROXY_PUT(F, Arg1, Arg2, Arg3, Mod, P),
        F(Arg1, Arg2, #proxy_mp_tree{mod = Mod} = P) ->
               Mod:proxy_put(F, ?MODULE, {Arg1, Arg2, Arg3}, P)).

-define(PROXY_ITER(F, Mod, I),
        F(#proxy_mp_tree_iter{mod = Mod} = I) -> Mod:proxy_iter(F, ?MODULE, I)).

-define(PROXY_ITER(F, Arg1, Mod, I),
        F(Arg1, #proxy_mp_tree_iter{mod = Mod} = I) ->
               Mod:proxy_iter(F, ?MODULE, {Arg1}, I)).

-define(PROXY_ITER(F, Arg1, Arg2, Mod, I),
        F(Arg1, Arg2, #proxy_mp_tree_iter{mod = Mod} = I) ->
               Mod:proxy_iter(F, ?MODULE, {Arg1, Arg2}, I)).
