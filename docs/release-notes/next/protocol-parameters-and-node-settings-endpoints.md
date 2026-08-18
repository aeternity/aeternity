* Added `GET /v3/protocol-parameters`: the consensus parameters (gas costs, name-auction fee/timeout
  tables, allowed VM/ABI versions, protocol-effective heights) needed to build a valid transaction,
  covering the protocol effective at the current top block and any later one whose fork height has not
  been reached yet. Grouped under `http.endpoints.node_info`, on by default.
* Added `GET /v3/node-settings`: this node's local operator policy (miner gas price, mempool limits,
  gas ceilings) - not consensus rules, and may differ between nodes. Grouped under the new
  `http.endpoints.node_settings`, on by default and independently disableable from `node_info`.
* Both endpoints report aettos amounts as decimal strings (values reach ~2^69, past what a JSON number
  survives in an IEEE-754 client) and support `int-as-string=true` for every other numeric field.
