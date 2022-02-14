* Introduces a new HTTP endpoint for fetching peer pool stats: connected
  (inbound and outbound), available for a new connection (verified and not yet
  verified) and blocked peers. This is part of the `node-operator` group that
  is bound to the internal interface and disabled by default. This new
  endpoint is intended for the node operator to be monitoring its node's
  connectivity.
