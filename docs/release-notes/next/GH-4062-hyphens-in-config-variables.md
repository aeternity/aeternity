* While it should be possible to override config values using OS environment variables,
  this didn't work for e.g. `http:endpoints:dry-run`, since the name contains a hyphen.
  Generally, it didn't work for any children of `http:endpoints`, since that schema subtree
  was improperly structured. This has been fixed, and for any config variable whose name contains
  a hyphen, the corresponding OS environment variable should replace any hyphen with an underscore:
  `AE__HTTP__ENDPOINTS__DRY_RUN=true`.
* The config variable `http:endpoints:node_operator` has been changed to `node-operator`, since
  this is what was expected by the application code. Due to the structural error above, it was
  possible to specify `node-operator` even before, and this is the only thing that would have worked.
  With the corrected structure and name change, such a setting will also be properly validated.
