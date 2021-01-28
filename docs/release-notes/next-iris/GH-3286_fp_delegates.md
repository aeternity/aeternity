* Revisits the State Channel delegates: so far they were a shared list for
  both participants. From Iris on, delegates are per peer: there is a list of
  delegates for the `initiator` and another one for the `responder`. Old
  channel objects can still be used but users are strongly recomended to reset
  their `delegates` list if they had any.
