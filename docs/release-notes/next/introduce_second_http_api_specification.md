* Introduces a second HTTP specification. This is to provide the new `OAS3`
  API under `/v3/` while we keep the `Swagger v2` API untacked under `/v2/`.
  This would provide a smooth transition from old specification to the new
  one. The new API is to be finalized with `iris` hardfork, please do not
  depend on it yet.
* Expands the `/api` endpoint with an option: while simply calling `/api`
  keeps it old behaviour, calling `/api?oas3` would provide the new `OAS3`
  spec instead.
