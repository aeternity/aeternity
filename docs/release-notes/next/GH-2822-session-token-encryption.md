* SC clients will now receive a token which will be required for reestablishing a channel or
reconnecting to an existing FSM, the token must be persisted and kept secret by the client.
* Removes the user provided password from the state channel API
* Removes the special channel reconnect transaction -
reestablishing a channel may now result in reconnecting to an existing FSM
* This release ships with backwards incompatible changes to the client WS API. 
Make sure an compatible SDK version is used when using state channels. Existing users of SC should
close their pre v5.2.0 channels and reopen them after v5.2.0. After v5.2.0 reconnection requests won't
require signing a transaction.
