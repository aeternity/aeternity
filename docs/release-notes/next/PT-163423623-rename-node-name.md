* Changes the node name from epoch@localhost to aeternity@localhost. This impacts persisted database (the node name is stored in it).
  In order to use persisted database created before this release, new tool `rename_db` must be used
  It takes path to `schema.DAT` file as an argument and the file `schema.DAT` can be found in the persisted database directory.
  Example usage: `./bin/aeternity rename_db ~/aeternity/node/my-old-db-path/mnesia/schema.DAT`.
