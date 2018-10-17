%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Behaviour module for nameing cache
%%% @end
%%%=============================================================================

-module(aens_cache).

-callback serialization_type() -> atom().

-callback serialization_template(non_neg_integer()) -> list().

-callback deserialize_from_fields(non_neg_integer(), binary(), list()) -> term().

