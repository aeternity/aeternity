* A micro-block candidate build that keeps failing is no longer retried flat out. Each
  failure respawned the worker at once, so an error that persists - the top's state trees
  not being readable, a worker that crashes on the same input - spun at around 78000
  rebuilds a second and rotated through the whole of `aeternity.log` in seconds. The first
  failure still retries immediately, consecutive ones 100 ms apart doubling to 5 s.
