* Fixed a HTTP API issue: in `/v2/` when fetching oracle queries, the client
  can specify if they want currently `open`, already `closed` or simply `all`
  queries for that oracle. If no parameter was provided, the endpoint crashed.
  Now if nothing is specified, `all` queries are returned as this is the
  default behaviour for `/v3/`.
