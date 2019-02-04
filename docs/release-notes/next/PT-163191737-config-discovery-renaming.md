* Changes user config discovery paths, i.e. the node is looking for the user config in:
  - AETERNITY_CONFIG environment variable instead of EPOCH_CONFIG,
  - ~/.aeternity/aeternity/aeternity.yaml file instead of ~/.epoch/epoch/epoch.yaml,
  - ${AETERNITY_TOP}/aeternity.yaml file instead of ${AETERNITY_TOP}/epoch.yaml.
  Backwards compatibility is kept for now, so user config defined in old locations will be working until the next major release.
