* Improved checking and defaults handling of mininum_depth parameters in State Channels.
  Also, `"minimum_depth_strategy": "plain"` is supported besides the default, adaptive,
  `txfee` strategy. With `plain`, the confirmation time will always be the number of blocks
  specified in `minimum_depth`.
  
