* The log directory now comes from the `setup` application's `log_dir` alone;
  lager's `log_root` is ignored and the path is made absolute at startup. The
  packaged default (`$HOME/log`) is unchanged. `setup` must now start before
  `lager`, which the release boot order does; a node starting them the other way
  round stops with `lager_started_before_setup`.
