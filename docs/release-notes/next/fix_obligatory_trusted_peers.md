* Fixes a bug: there are special trusted peers that are never deleted nor
  downgraded. Those are quite unique as the node consideres them to be
  available. One can pick their own set of trusted peers but this was not
  really effective: the default peers were always included as a bonus.
  This PR fixes it: if the new config flag is set to `false` - the defaults
  are omited. The flag is called `include_default_peers` and by default it
  is true.
