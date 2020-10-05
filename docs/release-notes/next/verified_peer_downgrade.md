* Improves sync's peer propagation: peers are split into verified and
  unverified. We had allowed a grace period for peers to be down while the
  node still considers them as verified and broadcasts them as such. Once a
  verified peer is being detected as down, it enters a standby state that
  allows them to stay verified.
  This PR tightens the process - once a peer is being detected to be
  unreachable - it is being downgraded from verified to unverified
  immediately. The processing of unverified being deleted is left unchanged -
  they still enter the standby mode for a few times before being removed.
