* Add support for protected contract calls. Making a contract call with the named
  argument `protected` set to `true` wraps the result of the call in an
  `option` type, returning `Some(res)` if the call succeeds with result `res`
  and `None` if the call fails for any reason.
