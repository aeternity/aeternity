* State Channels: Changed calculation of block confirmation times in to be dynamic.
  Previously the confirmation times would be static for all relevant transactions
  in a channel. This has been changed to calculate the block confirmation time
  based on the chosen strategy. For more information check the protocol
  documentation.
