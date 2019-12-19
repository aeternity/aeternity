* Refactor fetching of (forward) generations. This avoids a slow check for
  chain inclusion and will speedup `v2/generations/height/<height>` and
  `v2/generations/hash/<kh_...>`
