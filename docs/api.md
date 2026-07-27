# Introduction

Aeternity node API is documented in the [protocol repository](https://github.com/aeternity/protocol/blob/master/node/api/README.md).

Swagger API documentation is available online at [Aeternity node API documentation](https://api-docs.aeternity.io)

# Response headers

## `X-Ae-Height`

Every HTTP API response — external, internal and Rosetta, successful or not —
carries an `X-Ae-Height` header holding the height of the node's chain top at
the moment the request was accepted.

The marker is read before the request handler touches any chain data, so the
response body always reflects a chain state **at or after** the advertised
height. Clients may treat the value as a lower bound.

It exists so that a client talking to a pool of nodes behind a load balancer can
tell an answer computed from an up-to-date chain from one computed by a lagging
or still syncing node. Comparing the header against the highest value seen so
far lets the client discard a stale answer outright, instead of retrying blindly
and adding load to nodes that are already behind. Because the header is present
on error responses too, a `404` for a not-yet-mined transaction also says how
far the answering node actually is.

Resolution is one generation: micro blocks applied within the current generation
do not change the value. That keeps the header cache friendly for reverse
proxies sitting in front of the node — it changes about once per key block.

The header is omitted while the node's chain is not readable yet, e.g. early in
startup. Clients should treat its absence as "unknown".

Browsers can read the header on cross-origin requests: it is listed in
`access-control-expose-headers` whenever CORS headers are sent (see the
`http.cors` configuration section).
