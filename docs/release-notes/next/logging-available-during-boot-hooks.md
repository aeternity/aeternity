* Boot-time configuration errors are logged again. Reordering `setup` before
  `lager` left the boot hooks that read and validate the config (100-110)
  running before `lager` was started at hook 200, so a bad config key surfaced
  only as `validation_failed`. `lager` now starts as soon as the log directory
  and levels are settled, and the config validator falls back to `error_logger`
  for the two hooks that still precede it - that fallback reaches the console
  (`log/erlang.log.N` for a daemonized node, stdout otherwise), not
  `log/aeternity.log`.
* A plugin that fails to start is reported at `error` level, instead of being
  folded into the `info` line that reports the result either way.
