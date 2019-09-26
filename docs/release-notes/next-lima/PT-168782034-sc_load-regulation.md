* Load regulation for state channel set up was corrected so that requests release the job scheduler sooner,
  thereby allowing for more channels than `regulators:sc_ws_handler:counter`. A config value,
  `channels:max_count` (default: 1000) is introduced, to limit the total number of active channels on a node.
