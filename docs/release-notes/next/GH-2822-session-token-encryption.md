* SC clients will now receive a token which will be required for reestablishing a channel or
reconnecting to an existing FSM, the token must be persisted and kept secret by the client.
* Removes the user provided password from the state channel API
* Removes the special channel reconnect transaction -
reestablishing a channel may now result in reconnecting to an existing FSM
