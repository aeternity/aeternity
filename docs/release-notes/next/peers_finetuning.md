* There used to be multiple copies of the same peers in peers pool. This
  resulted in propagation of same peers with different Peer IDs through the
  network. Now duplicates are cleaned up.
* Some further peers fine tuning: we used to treat peers that refuse
  connecting to us as they were malicious. This was the case no matter if
  peers were active and part of the network and only simply refusing to open a
  `enoise` connection to our node (most likely because their inbound pool is
  staturated already). This is now changed and if the node is responding but
  refusing to open an `enoise` connection - we consider it still a valid and
  verified participant of the network.
