- The [```channel_close_mutual```](https://github.com/aeternity/protocol/blob/master/channels/ON-CHAIN.md#channel_close_mutual) 
transaction can now be performed while the channel is not active on-chain (The channel solo closing sequence had been initiated and the channel is not yet closed), 
this is a consensus breaking change available after the Lima fork.
- The state channel FSM after the Lima fork accepts a shutdown message while the channel is being closed unilaterally.
