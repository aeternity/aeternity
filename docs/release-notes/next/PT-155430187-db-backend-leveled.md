* Add beta support for new database backend [leveled](https://github.com/martinsumner/leveled)
  * Enable by setting the configuration `chain->db_backend` in `aeternity.yaml` to `leveled`
  * When first enabling a new database backend you must rename your database folder, or delete it, and resynchronize the chain
