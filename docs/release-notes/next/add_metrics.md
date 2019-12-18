* Added new metrics to ae.epoch.aemon namespace
** `block.propagation_time.key`   : Time until key-blocks reached this node in milliseconds
** `block.propagation_time.micro` : Time until micro-blocks reached this node in milliseconds
** `block.time_since_prev.key`    : Time between key-blocks in milliseconds
** `block.time_since_prev.micro`  : Time between micro-blocks in milliseconds
** `chain.top.difficulty`         : Difficulty of the top block
** `forks.micro.count`            : Count of observed micro-forks
** `forks.micro.height`           : Height difference of observed micro-forks
* Increased histogram timespan to 1 hour for some aemon metrics
