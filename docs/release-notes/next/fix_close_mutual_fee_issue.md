* Fixes a State Channel WebSocket API issue: if a user provided a fee, it was
  ignored
* Fixes a State Channel WebSocket API issue: when participants try closing a
  channel with insufficient channel balance to cover the fee, a proper error
  is being sent.
