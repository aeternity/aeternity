* Allows sending and receiving generic messages in State Channels in any FSM
  state. Until this - generic messages were allowed only in `open` state.
  `channel_id` is part of the message body and if it is unknown - the
  temporary one is used instead. This is breaking off-chain consesus.
