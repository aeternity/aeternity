* Changes how a meta transaction TTL's is being validated: so far it used to
  be the outermost transaction's ttl that was taken into account, now it is
  the innermost one instead. Meta transactions no longer have TTL.
