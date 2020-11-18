* Fixes a bug: there are special trusted peers that are never deleted nor
  downgraded. Those are quite unique as the node consideres them to be
  available. One can pick their own set of trusted peers but this was not
  really effective: the default peers were always included as a bonus.
  This PR fixes it: if there are user provided peers, the defaults are
  omited.
