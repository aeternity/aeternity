* The state channel chain watcher now watches the chain continuously
* The state channel FSM stays open during the closing phase
* Client WS API for solo-closing, slash and settle
* SC FSM automatically detects when a slash is needed
* SC FSM reports to client any time the on-chain channel state changes
* The SC watcher is now fork-aware
