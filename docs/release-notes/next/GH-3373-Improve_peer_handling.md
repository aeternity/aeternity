* Changes the default value and significantly speeds up the TCP probes. Those
  used to be done roughly once every 2 minutes while now they are executed
  every second. Note that this is done only if there are peers to be checked:
  if a peer is missing, there is a grace period in which we mark it as
  'suspended', waiting for its return. If all peers are checked, we wait
  longer - until either a new peer is added or there is a not-suspended peer.
  Basically the probes come on demand. Even despite the probes are fast, there
  is a maximum size of currently ongoing probes.
* Fixes the process of probing verified peers. Until now we were probing only
  unverified peers. If a peer responds to the probe - we consider it verified.
  We downgrade a verified peer only after we fail connecting to it. This
  resulted in amounting peers that used to be verified at one point of time.
  This is fixed now as we probe verified peers as well. What is more is, that
  this PR plays really well with PR #3365.  This means that if a peer goes
  down, we will find it significantly sooner and will downgrade it as an
  unverified one. This is important as a nodes shares to other nodes only
  peers one considers to be verified.
