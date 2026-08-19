* `POST /v3/key-blocks` accepts a body without `flags` again. The field had become
  required on the `KeyBlock` schema, which is also that endpoint's request body, so
  a miner building the body itself rather than posting back
  `GET /v3/key-blocks/pending` was answered `400` where 7.2.2 accepted the block.
  The node defaults the field when it is absent; responses are unchanged.
* `flags` is now declared on `MicroBlockHeader`, which has always returned it, and
  on both schemas in the deprecated Swagger 2 specification.
