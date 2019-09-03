* State Channels: It is now possible to abort a signing request by replying with an error code.
* State Channels: The FSMs now stay alive until a Close Mutual (shutdown) has been confirmed on-chain
  It is also possible to abort/reject a Close Mutual by rejecting the signing request.
* State Channels: The chain watcher was not restarted after leave/reestablish. This has been fixed.
