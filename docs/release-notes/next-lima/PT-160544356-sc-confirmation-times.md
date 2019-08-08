- Changed calculation of block confirmation times in State Channels to be dynamic.
  Previously the confirmation times would be static for all relevant transactions
  in a channel. This has been changed to incorporate the transaction fee and
  a new `channel_accept` parameter `minimum_depth_factor` which can be used to
  further adapt the confirmations times used in a channel.
  **This change is backwards-incompatible because of changes in the noise
  message protocol between state channel processes.**
