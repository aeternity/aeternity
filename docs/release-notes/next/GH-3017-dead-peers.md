* Makes some peer pool parameters configurable:
** `gossiped_peers_count`
** `select_verified_peer_probability`
** `max_update_lapse`
** `standby_times`
** `max_rejections`
* Sets default `select_verified_peer_probability` to 1.0 to make sure a peer is selected from the verified pool first.
* Sets default `max_update_lapse` to 2592000000 (30 days).
