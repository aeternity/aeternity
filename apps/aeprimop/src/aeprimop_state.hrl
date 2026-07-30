%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-record(state, { trees      :: aec_trees:trees()
               , height     :: non_neg_integer()
               , cache      :: aec_state_db:state_db()
               , env        :: dict:dict()
               , tx_env     :: aetx_env:env()
               , protocol   :: aec_hard_forks:protocol_vsn()
               }).
