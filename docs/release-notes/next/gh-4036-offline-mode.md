* A new runtime mode has been added: 'offline', which disables mining and sync, but leaves HTTP endpoints active. Controlled via the command `bin/aeternity offline [on | off | status]` (GH #4036)

* The method `debug/channels/fsm-count` has been added as a debug HTTP endpoint (GH #4029)

* It is now possible to use a custom file name for pre-funded accounts for testing, e.g. when using dev mode. (GH #4025)

* Corrected nonce handling in tx events generated from oracle queries (GH #4027)

* Internal fault escalation for the core modules was improperly configured, which could lead to protracted error conditions. (GH #4023)

* When loading plugins during development, the node now automatically loads dependent applications (provided the plugin is built using rebar3) (GH #4020)
