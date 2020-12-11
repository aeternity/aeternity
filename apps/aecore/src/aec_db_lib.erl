-module(aec_db_lib).
-export([rocksdb_write/3]).

%% For fault injection
rocksdb_write(Handle, Batch, Opts) ->
    rocksdb:write(Handle, Batch, Opts).
