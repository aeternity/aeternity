- The password used for encrypting the persisted cached state channel state is now 
required in order to open or reestablish a state channel. In case a state chanel was opened before the lima fork the
user MUST provide the default password - "correct horse battery staple". Not providing the password in the websocket client
will yield two errors - "Invalid password" and "Required since lima fork".
