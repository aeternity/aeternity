* Changes the structure of off-chain transactions: off-chain updates are moved
  out of it so the on-chain world is agnostic to the off-chain update protocol
  being used as long as force progress expectations are met. This is impacts
  consesus and takes action in Fortuna hard fork
* Off-chain updates introduced to State Channels noise session protocol. This
  impacts off-chain protocol.
* Revisited JSON serialization of off-chain updates to be in sync with the
  corresponding messages for updates' creation. This impacts API.
