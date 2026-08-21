* The Erlang RocksDB binding moved from the `barrel-db` fork to `emqx`, with
  `mnesia_rocksdb` updated alongside it. Both forks vendor the same RocksDB 7.7.3
  and differ only in build portability, so the chain database format is unchanged
  and no migration is involved. Building from source on Ubuntu now also needs
  `libtool` (see `docs/build.md`); the official Docker image links the system
  RocksDB and is unaffected either way.
* The `mnesia_rocksdb` update fixes wrong index preparation in `mrdb:index_read_/3`.
