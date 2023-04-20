* To eliminate the risk of garbage-collecting a competing fork, which is later evicted,
  the garbage collector sweeps start below the fork resistance depth (or a user-configured depth)
  below the top.
