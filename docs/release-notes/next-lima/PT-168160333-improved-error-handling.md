* State Channels: It is now possible to abort a signing request by replying with an error code.
* State Channels: The FSMs now stay alive until a Close Mutual (shutdown) has been confirmed on-chain
  It is also possible to abort/reject a Close Mutual by rejecting the signing request.
  See https://github.com/aeternity/protocol/blob/master/node/api/channel_ws_api.md#signing-error-replies
* State Channels: On-chain tx monitoring was broken after leave/reestablish. This has been fixed.
