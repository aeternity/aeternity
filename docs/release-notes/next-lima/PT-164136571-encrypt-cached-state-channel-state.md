* Introduces on-disk state cache encryption for State Channels.
  * When a user leaves a state channel the off-chain state will be persisted on disk and protected 
    with a password. The password is provided by the user when opening a channel using the `state_password` 
    parameter.
  * When re-establishing the channel the same password **MUST** be provided, otherwise 
    the operation will fail with error code `invalid_password`.
  * The password **MUST** be at least 6 characters long. Providing a shorter password 
    will fail with error code `invalid_password`.
  * The password may be changed anytime by the user through the websocket connection after the channel 
    has been opened. Please consult the documentation for more details. This operation is only allowed 
    when the channel is established. If the user has left the channel, the channel must be re-established
    first before changing the password.
  * Until the lima fork the password will be optional. By not providing a password a default value is used:
    `correct horse battery staple`. After the Lima fork the password will become mandatory. Pre-Lima off-chain states 
    will be encrypted with the default password.
  * Because the password used for encrypting the persisted state cache is mandatory after the Lima fork, 
    state channels which were opened before Lima use the default password. 
	Not providing the password in the websocket connection will result in a `missing_field` error.
  * Keep in mind that an adversary with direct RAM access to the node may still steal the off-chain state.
    This change only protects the state against an adversary with direct disk access.