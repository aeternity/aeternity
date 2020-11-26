* Introduces persistence of the peer pool: if the node flag for disc
  persistence is set, after a node restart old peers are loaded from the DB.
  Trusted peers are provided to the node from the config file so they are already
  persisted. Since the time being off is unknown, all peers are loaded as
  `unverified`, even if they had been `verified` before the node stop.
