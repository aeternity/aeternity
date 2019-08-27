- When a user leaves a state channel, the off chain state will be persisted on disk and protected with a user provided password - please keep in mind that an adversary
with direct RAM access to the node may still steal the off chain state - this change only protects the state against an adversary with direct disk access.
- The encryption password is provided by the user when opening a channel using the `state_password` parameter, when reestablishing the channel the same
password MUST be provided, otherwise the operation will fail with error code "Invalid password".
- The encryption password must be at least 6 characters long - providing a shorter password will fail with error code "Invalid password"
- The encryption password may be changed anytime by the websocket client after the channel has been opened - please consult the documentation for more details. 
This operation is only allowed when the channel is established - If we had left the channel, the user needs to reestablish the channel with the old password before changing the password to a newer one.
- Until the lima fork the password will be optional - by not providing a password a default is used - "correct horse battery staple". Keep in mind that providing the password
will become mandatory after the lima fork. Old v4.* offchain states will be encrypted with the default password.
- Sice lima the password used for encrypting the persisted cached state channel state is mandatory in order to open or reestablish a state channel. In case a state chanel was opened before the lima fork the
user MUST provide the default password - "correct horse battery staple". Not providing the password in the websocket will result in an "Missing field" error.
