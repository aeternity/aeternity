* Splits the old VM-version into VM-version and ABI-version. Contract calls and Oracles only deal with ABI-version. This
  changes the HTTP API. We have two VM-versions (SOPHIA_1 = Roma, and SOPHIA_2 = Minerva), but only one ABI-version.
