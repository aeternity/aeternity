%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Records for entities that are used by dispatcher in aec
%%% @end
%%%=============================================================================

-record(commitment,
        {hash    :: binary(),
         owner   :: pubkey(),
         created :: height(),
         expires :: height()
         }).

-type name_status() :: claimed | revoked.

-record(name,
        {hash          :: binary(),
         owner         :: pubkey(),
         expires       :: height(),
         status        :: name_status(),
         ttl      = 0  :: integer(),
         pointers = [] :: list()}).
