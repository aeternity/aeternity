* Off-chain updates can now still be completed even if one of the Websocket clients is off-line
  If the originator manages to get the initial update request co-signed 'out-of-band', the
  responding FSM will proceed as if it received a successful signing response from the client.
