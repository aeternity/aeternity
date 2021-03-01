* Introduces a new query param in relevant HTTP APIs. It is called
  `int-to-str` and specifies that all integers in the response shall be
  represented as strings instead. This will allow SDKs to pick their
  preference for the data representation and will help them solve precision
  issues. This applies only for v3 API that supports `oas3` and is not
  supported by the `swagger v2` API
* Allows passing strings instead of integers in `post` requests for `debug`
  APIs that are convenience for testing and developing SDKs. 
