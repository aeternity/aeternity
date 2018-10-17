%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Behaviour module for oracles' cache
%%% @end
%%%=============================================================================

-module(aeo_cache).

-callback serialization_type() -> atom().

-callback serialization_template(non_neg_integer()) -> list().

-callback deserialize_from_fields(non_neg_integer(), binary(), list()) -> term().

-callback ttl(term()) -> integer().

