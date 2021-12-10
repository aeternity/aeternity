* Change the micro block gas limit to use actual _used gas_ instead of upper
  _gas limit_. This makes the network work better when contract calls are
  overestimated and allow for more transactions to fit in each block.
