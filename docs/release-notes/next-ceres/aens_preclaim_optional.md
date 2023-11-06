* Make NamePreclaimTx optional. Since the introduction of auctions, the
  front-running protection offered by the 'PreClaim -> Claim' flow is no
  longer as important. To simplify name registrations (or the start of a
  name auction) we now allow NameClaimTx without a previous NamePreclaimTx.
  In this case we set the `NameSalt` to `0`.
